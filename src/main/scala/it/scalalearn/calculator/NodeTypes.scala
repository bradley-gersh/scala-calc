package it.scalalearn.calculator

class Node()

case class Expression() extends Node

case class Value(value: Double)

case class Product(expr1: Expression, op: Token, expr2: Expression) {
}

case class Sum(expr1: Expression, op: Token, expr2: Expression) {
}
