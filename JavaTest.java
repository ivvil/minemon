import java.io.IOException;

public class JavaTest {
	public static void main(String[] args) {
		try {
			System.getProperties().storeToXML(System.out, null, "UTF-8");
		} catch (IOException e) {
			System.exit(1);
		}

		System.exit(0);
	}
}
