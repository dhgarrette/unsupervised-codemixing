package dhg.ccg.tagdict

trait StartEndTags[Tag] {
  def startTag: Tag
  def endTag: Tag
  def swap: StartEndTags[Tag] = SimpleStartEndTags(endTag, startTag)
}
case class SimpleStartEndTags[Tag](startTag: Tag, endTag: Tag) extends StartEndTags[Tag]
