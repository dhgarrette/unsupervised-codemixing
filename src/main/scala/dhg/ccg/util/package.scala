package dhg.ccg.util

object `package` {

  case class TempLogDouble(v: Double) {
	  def <+>(o: TempLogDouble) = TempLogDouble(v + o.v)
    def <*>(o: TempLogDouble) = TempLogDouble(v * o.v)
    def </>(o: TempLogDouble) = TempLogDouble(v / o.v)
  }

}
