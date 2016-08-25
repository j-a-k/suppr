@SuppressWarnings("PMD.CommentRequired")
public class Foo
{
    private int val;

    @SuppressWarnings("PMD.CommentRequired") //This is a test case
    public Foo(int val)
    {
        this.val = val;
    }

    @SuppressWarnings("Foo") //BAR
    public int getVal()
    {
        return val;
    }

}