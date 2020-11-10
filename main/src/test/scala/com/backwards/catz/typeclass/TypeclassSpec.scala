package com.backwards.catz.typeclass

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.catz.typeclass.thirdparty.Train

class TypeclassSpec extends AnyWordSpec with Matchers {
  trait Vehicle {
    def drive(): String
  }

  case class Car(make: String) extends Vehicle {
    override def drive(): String = s"driving a $make car"
  }

  case class Bus(color: String) extends Vehicle {
    override def drive(): String = s"driving a $color bus"
  }

  class Person {
    def drive(vehicle: Vehicle): Unit = println(vehicle.drive())
  }

  val person = new Person

  "Typeclass" should {
    "step 1" in {
      val car = Car("ford")
      person.drive(car) // driving a ford car

      val bus = Bus("yellow")
      person.drive(bus) // driving a yellow bus

      val train = thirdparty.Train("electric")
      "person.drive(train)" mustNot compile
    }

    "step 2 - modify train to make it extend our Vehicle so we need to subclass the Train" in {
      class MyTrain(category: String) extends thirdparty.Train(category) with Vehicle {
        def drive(): String = s"driving $category train"
      }

      val mytrain = new MyTrain("electric")
      person.drive(mytrain) // driving electric train
    }

    "step 3 - third party train is final to we can't extend, we need an adapter" in {
      class MyTrain(underlying: thirdparty.Train) extends Vehicle {
        override def drive(): String = s"driving ${underlying.category} train"
      }

      val mytrain = new MyTrain(thirdparty.Train("electric"))
      person.drive(mytrain) // driving electric train
    }

    "step 4 - implicit class" in {
      implicit class MyTrain(underlying: thirdparty.Train) extends Vehicle {
        override def drive(): String = s"driving ${underlying.category} train"
      }

      def driveAndReturn(vehicle: Vehicle): Vehicle = {
        vehicle.drive()
        vehicle
      }

      val train: thirdparty.Train = thirdparty.Train("electric")

      val drivenTrain: Vehicle = driveAndReturn(train)
      // Problem - drivenTrain is no longer a train, just a Vehicle.
      // We could cast:
      val drivenTrainHack = driveAndReturn(train).asInstanceOf[MyTrain]
    }

    "step 5 - typeclasses" should {
      trait VehicleLike[A] {
        def drive(a: A): String
      }

      object VehicleLike {
        def apply[A: VehicleLike]: VehicleLike[A] = implicitly

        // Common pattern to have some common implementations
        implicit val vehicleLikeCar: VehicleLike[Car] =
          (car: Car) => s"driving a ${car.make} car"

        implicit val vehicleLikeBus: VehicleLike[Bus] =
          (bus: Bus) => s"driving a ${bus.color} bus"
      }

      object Dealership {
        def driveAndReturn[A: VehicleLike](vehicle: A): A = {
          println(VehicleLike[A].drive(vehicle))
          vehicle
        }
      }

      """
        |In "classic" object oriented design we describe behaviour in an interface/trait and define concrete implementations of that interface.
        |In other words we say a Car is a Vehicle.
        |Type classes approach this from a slightly different angle.
        |We define a trait which says something of type A should be capable of being treated as a Vehicle.
        |We then create concrete implementations of this contract for each type of Vehicle we are interested in.""".stripMargin in {
        val ford: Car = Car("ford")

        val drivenFord: Car = Dealership.driveAndReturn(ford) // still a Car
        drivenFord.make mustBe "ford"

        // And now to use our third party Train
        implicit val vehicleLikeTrain: VehicleLike[Train] =
          (train: Train) => s"driving ${train.category} train"

        val train = thirdparty.Train("electric")
        val drivenTrain: thirdparty.Train = Dealership.driveAndReturn(train)
        drivenTrain.category mustBe "electric"
      }

      "Compose - We want to pass a list of cars to driveAndReturn" should {
        "step 5a (bad) - we could modify driveAndReturn to accept a list of vehicles" in {
          def driveAndReturn[A](vehicles: List[A])(implicit vehicleLike: VehicleLike[A]): List[A] = ???
        }

        "step 5b (bad) - We could keep the existing driveAndReturn signature but create a type class implementation for a List of Cars" in {
          implicit val VehicleLikeCarList: VehicleLike[List[Car]] =
            (cars: List[Car]) => cars.map(car => s"driving a ${car.make} car").mkString(System.lineSeparator)
        }

        """
          |step 5c (good) - Compose two type classes
          |We will leave the original vehicleLikeCar unchanged and create a new type class for Lists.
          |But we won't create it for List[Car], but List[A].
          |Our new type class will behave in a similar way to driveAndReturn, it will also look for a type class implementation for A""".stripMargin in {
          implicit def vehicleLikeList[A: VehicleLike]: VehicleLike[List[A]] =
            (as: List[A]) => as.map(a => VehicleLike[A].drive(a)).mkString(System.lineSeparator)

          val cars = List(Car("ford"), Car("BMW"), Car("VW"))
          val drivenCars: List[Car] = Dealership.driveAndReturn(cars)

          drivenCars mustBe cars

          /*
          But it gets better, as we have a type class for lists and a type class for buses (and tanks!)
          */
        }

        "step 5d - handle options" in {
          implicit def vehicleLikeOption[A: VehicleLike]: VehicleLike[Option[A]] =
            (a: Option[A]) => a.map(VehicleLike[A].drive).getOrElse("Nothing to drive")

          val someCar: Option[Car] = Some(Car("ford"))
          val noCar: Option[Car] = None

          Dealership.driveAndReturn(someCar) mustBe someCar
          Dealership.driveAndReturn(noCar) mustBe noCar
        }

        "step 5e - handle list of options and option of list - we'll copy the implicits from above as they are not in this test scope" in {
          implicit def vehicleLikeList[A: VehicleLike]: VehicleLike[List[A]] =
            (as: List[A]) => as.map(a => VehicleLike[A].drive(a)).mkString(System.lineSeparator)

          implicit def vehicleLikeOption[A: VehicleLike]: VehicleLike[Option[A]] =
            (a: Option[A]) => a.map(VehicleLike[A].drive).getOrElse("Nothing to drive")

          val listOfOptionalCars: List[Option[Car]] = List(Some(Car("ford")), Some(Car("VW")), None)
          Dealership.driveAndReturn(listOfOptionalCars) mustBe listOfOptionalCars

          val optionalListOfCars: Option[List[Car]] = Some(List(Car("ford")))
          Dealership.driveAndReturn(optionalListOfCars) mustBe optionalListOfCars

        }
      }
    }
  }
}