{ \singlespace
\tiny
\begin{verbatim}
public class JavaSemiCount implements SizeMeasure {
    public String getName() { return "Java Semicolon Counter"; }
    public String getCLIArg() { return "javasemi"; }
    public OutputFormat[] getOutputFormats() {
        OutputFormat[] f = new OutputFormat[1];
        f[0] = new PlainOutputFormat();
        return f;
    }

    private static class PlainOutputFormat 
      implements OutputFormat, TotalPrinter, DiffPrinter {
        private PrintWriter out;
        private String n1, n2;
        public String getName() { return "Plain output format"; }
        public String getCLIArg() { return "plain"; }
        public TotalPrinter getTotalPrinter() { return this; }
        public DiffPrinter getDiffPrinter() { return this; }
        public void printTotal(InputStream in) 
          throws IOException {
            out.println(countSemis(in) + 
                        " semicolons in file " + n1);
        }
        public void printDiff(InputStream oldS, InputStream newS) 
          throws IOException {
            int diff = countSemis(newS) - countSemis(oldS);
            out.println("semi difference for files " + n1 + " and " +
                        n2 + " is " + diff);
        }
        private int countSemis(InputStream in) 
          throws IOException {
            BufferedReader reader = 
                new BufferedReader(new InputStreamReader(in));
            int c, count = 0;
            while ((c = reader.read()) != -1) {
                if (c == ';') 
                    count++;
            }
            return count;
        }
    }
}
\end{verbatim}
%\doublespace
    }
