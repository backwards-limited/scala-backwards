/*
package com.backwards.quill

import java.sql.{Connection, Statement}
import io.getquill.{PostgresAsyncContext, PostgresJdbcContext, SnakeCase}
import org.postgresql.ds.PGSimpleDataSource
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.dimafeng.testcontainers.ForAllTestContainer
import io.getquill._
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import org.postgresql.ds.PGSimpleDataSource
import com.backwards.container.PostgreSQLContainer

// docker exec -it strange_mayer psql -Utest -a test -c 'select * from contact;'
// docker exec -it strange_mayer psql -Utest -a test -c 'create table Foo(personId int not null, phone varchar(20) not null, primary key (personId));'
class PostgresSpec extends AnyWordSpec with Matchers with ForAllTestContainer {
  override val container: PostgreSQLContainer =
    PostgreSQLContainer()

  override def afterStart(): Unit = {
    println("===> HERE")
    //container.container.execInContainer("psql -Utest -a test -c 'create table Foo(personId int not null, phone varchar(20) not null, primary key (personId));'")

    //container.container.execInContainer("psql", "postgresql://test:test@$localhost:5432/test", "-c", "create table Foo(personId int not null, phone varchar(20) not null, primary key (personId));")

    val connection: Connection = container.container.createConnection(container.jdbcUrl)

    val statement: Statement = connection.createStatement()

    statement.execute("create table Foo(personId int not null, phone varchar(20) not null, primary key (personId));")

    //container.container.execInContainer("""psql -Utest -a test -c 'create table Foo(personId int not null, phone varchar(20) not null, primary key (personId));'""")
  }

  "" should {
    "" in {
      println("===> TEST")
      //container.container.execInContainer("""psql "postgresql://test:test@$localhost:5432/test" -c 'create table Foo(personId int not null, phone varchar(20) not null, primary key (personId));'""")

      // export PGPASSWORD='test'; psql -d test -U test -c 'create table Foo(personId int not null, phone varchar(20) not null, primary key (personId));'

      // psql "postgresql://$DB_USER:$DB_PWD@$DB_SERVER/$DB_NAME"

      // psql "postgresql://test:test@$localhost:5432/test" -c 'create table Foo(personId int not null, phone varchar(20) not null, primary key (personId));'

      val ds = new PGSimpleDataSource()
      ds.setServerNames(Array("localhost"))
      ds.setPortNumbers(Array(container.mappedPort(5432)))
      ds.setDatabaseName(container.container.getDatabaseName)
      ds.setUser(container.username)
      ds.setPassword(container.password)

      val config = new HikariConfig()
      config.setDataSource(ds)

      val ctx: PostgresJdbcContext[LowerCase.type] = new PostgresJdbcContext(LowerCase, new HikariDataSource(config))

      contact(ctx)
      foo(ctx)
    }
  }

  def contact(ctx: PostgresJdbcContext[LowerCase.type]) = {
    println("====================> CONTACT")

    import ctx._

    val a = quote(query[Contact].insert(lift(Contact(999, "+1510488988"))))

    val v = ctx.run(a) // = 1 if the row was inserted 0 otherwise
    // INSERT INTO Contact (personId,phone) VALUES (?, ?)
    println(v)

    val q = quote(query[Contact])
    println(ctx.run(q))
  }

  def foo(ctx: PostgresJdbcContext[LowerCase.type]) = {
    println("====================> FOO")

    import ctx._

    val a = quote(query[Foo].insert(lift(Foo(111, "09483763"))))

    val v = ctx.run(a) // = 1 if the row was inserted 0 otherwise
    // INSERT INTO Contact (personId,phone) VALUES (?, ?)
    println(v)

    val q = quote(query[Foo])
    println(ctx.run(q))
  }
}

final case class Contact(personId: Int, phone: String)

final case class Foo(personId: Int, phone: String)*/
