package jameson
package enc

import java.io.Writer

import jameson.JPath

final class EncodingContext(val writer: Writer, private var _path: JPath, val options: EncodingOptions) {

  def path: JPath = _path

  def pathDown(name: String): Unit = {
    _path = _path / name
  }

  def pathDown(index: Int): Unit = {
    _path = _path / index
  }

  def pathUp(): Unit = {
    _path = _path.parent
  }
}
