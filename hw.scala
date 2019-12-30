import scala.collection.Seq

object Hi{
  case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)
  def asciiDisplay(root: TreeNode[String]): Seq[String]={
    def traverse(root: TreeNode[String], i:Int): Seq[String]={
      val rootstr =  List(" "*i + "+-" + (root.data:String)) 
      val childres = root.children.map((x:TreeNode[String]) => traverse(x,i+1):Seq[String])
      if(root.children == Nil || childres == Nil)
        return rootstr
      val res = rootstr ++ childres.reduce(_ ++ _)

      return res
    }
    return traverse(root,0)
  }
  def main(args: Array[String]){
    val root = TreeNode("Root",
        children = List(TreeNode("level1-1"),
                  TreeNode("level1-2"),
                          TreeNode("level1-3")));
    asciiDisplay(root).foreach(println);
  }
}

