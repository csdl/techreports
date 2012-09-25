\singlespace
\begin{verbatim}
package csdl.locc.java.parser;

public interface JavaLineParserVisitor
{
  public Object visit(CompilationUnit node, Object data);
  public Object visit(JavaClass node, Object data);
  public Object visit(InnerClass node, Object data);
  public Object visit(Interface node, Object data);
  public Object visit(Method node, Object data);
  public Object visit(AnnonClass node, Object data);
}
\end{verbatim}
\doublespace
