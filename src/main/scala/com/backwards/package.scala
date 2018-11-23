package com

package object backwards {
  type Or[Left, Right] = Left Either Right

  type |:[Left, Right] = Or[Left, Right]
}