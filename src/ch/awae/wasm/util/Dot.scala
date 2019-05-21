package ch.awae.wasm.util

import java.io.{BufferedWriter, File, FileWriter}
import java.util.UUID

object Dot {

  def apply(data:String, prefix:String):Unit ={
    val file = UUID.randomUUID.toString
    val writer = new BufferedWriter(new FileWriter(new File(file)))
    writer.write(data)
    writer.flush()
    writer.close()
    val process = new ProcessBuilder().command("dot", file, "-Tpdf", "-Gdpi=300").redirectOutput(new File(s"dot/$prefix-$file.pdf")).start()
    process.waitFor()
    println("exported " + prefix)
  }

}
