package jameson

sealed trait JLookup {
  def isUndefined: Boolean

  /** Attempts to retrieve the value of the property with the specified name. Returns
    * [[JUndefined]] if there is no such property or the subject is not an object.
    */
  def apply(name: String): JLookup

  /** Attempts to retrieve the array element at the specified index. Returns
    * [[JUndefined]] if there is no such element or the subject is not an array.
    */
  def apply(index: Int): JLookup

  /** Attempts to retrieve the value at the specified path relative to the subject.
    */
  def apply(path: JPath): JLookup = path.resolve(this)

  /** Attempts to retrieve the value of the property with the specified name. Returns
    * [[JUndefined]] if there is no such property or the subject is not an object.
    */
  def /(name: String): JLookup = apply(name)

  /** Attempts to retrieve the value of the property with the specified name. Returns
    * [[JUndefined]] if there is no such property or the subject is not an object.
    */
  def /(index: Int): JLookup = apply(index)
}

case object JUndefined extends JLookup {
  def isUndefined: Boolean = true
  def apply(name: String): JLookup = JUndefined
  def apply(index: Int): JLookup = JUndefined
}

sealed trait JValue extends JLookup {
  def isUndefined: Boolean = false
}
