import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;

public class ASServer extends UnicastRemoteObject implements ASIntf {

	private static final long serialVersionUID = 1L;
	private MyList<String> userList; // Usuarios leidos de archivo

	/**
	 * Constructor del Servidor de Autenticacion
	 */
	public ASServer() throws RemoteException {
		super();
		userList = new MyList<String>();
	}

	/**
	 * Permite saber si un usuario y contraseña es correcto
	 */
	public boolean isClient(String user, String key) throws RemoteException {
		String text;
		ListIterator<String> listIter = userList.iterator();
		while (listIter.hasNext()) {
			text = listIter.next();
			if (text.compareTo(user + ':' + key) == 0)
				return true;
		}
		return false;
	}

	private void getUsers(BufferedReader userReader) {
		String line;
		try {
			while ((line = userReader.readLine()) != null) {
				userList.add(line);
			}
			userReader.close();
		} catch (IOException e) {
			System.out.println("Error en la lectura de usuarios.");
		}
	}

	public static void main(String[] args) {

		String userFile = null; // Nombre de archivo de usuarios
		String port = null; // Puerto del servidor

		BufferedReader userReader = null; // Lector de usuarios
		ASServer authSystem = null; // Objeto servidor remoto

		System.out.println("Servidor de Autenticacion Iniciado.");

		/* Verificacion de argumentos */

		for (int i = 0; i < args.length; i = i + 2) {
			String argument = args[i];

			if (i + 1 > args.length) {
				System.out.println("Los argumentos estan incompletos.");
				System.exit(-1);
			}

			switch (argument) {
			case "-f":
				userFile = args[i + 1];
				try {
					userReader = new BufferedReader(new FileReader(userFile));
				} catch (IOException e) {
					System.out.println("El archivo de usuarios no puede abrirse.");
					System.exit(-1);
				}
				break;
			case "-p":
				port = args[i + 1];
				break;
			default:
				System.out.println("Se ha dado una opcion invalida.\nUso: " + "java a_rmifs -f usuarios -p puerto.");
				System.exit(-1);
				break;
			}
		}

		// Revisar que se hayan dado todos los argumentos obligatorios
		if (userFile == null || port == null) {
			System.out.println("No se otorgaron todas las opciones.\nUso: " + "java a_rmifs -f usuarios -p puerto.");
			System.exit(-1);
		}

		/* Configuracion de servidor */

		// RMI Registry Run
		try {
			LocateRegistry.createRegistry(Integer.parseInt(port));
			System.out.println("RMI Registry creado.");
		} catch (RemoteException e) {
			System.out.println("RMI Registry ya existe.");
		}

		// Generacion de Servidor
		try {
			authSystem = new ASServer();
		} catch (RemoteException e) {
			System.out.println("El objeto servidor remoto no pudo ser creado.");
			System.exit(-1);
		}
		authSystem.getUsers(userReader);

		// Anclaje de objeto a direccion
		try {
			Naming.rebind("rmi://localhost:" + port + "/ASServer", authSystem);
			System.out.println("Servidor de Autenticacion anclado.");
		} catch (RemoteException e) {
			System.out.println("El Servidor de Autenticacion no pudo ser anclado.");
			System.exit(-1);
		} catch (MalformedURLException e) {
			System.out.println("La direccion del servidor ha sido malformada.");
			System.exit(-1);
		}
	}
}
