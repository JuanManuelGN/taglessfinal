// Nombre del proyecto
name := "TaglessFinal"

// Versión del proyecto
version := "0.1.0"

// Versión de Scala
scalaVersion := "3.3.0"

// Dependencias del proyecto
libraryDependencies ++= Seq()

// Opciones del compilador de Scala
scalacOptions ++= Seq(
  "-deprecation",           // Muestra advertencias sobre características obsoletas
  "-feature",               // Muestra advertencias sobre el uso de características avanzadas
  "-unchecked",             // Verifica más advertencias durante la comprobación del tipo
  "-Xfatal-warnings"       // Trata las advertencias como errores
)
