import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.nio.file.Files;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.nio.file.Paths;

public class FSClient {

	/**
	 * A partir de un texto de un comando, devuelve el codigo del mismo y lo
	 * ejecuta. De ser invalida el comando devuelve -1.
	 */
	private static int commandParser(String[] s, String name, String key, FSIntf fileSystem) throws RemoteException {
		switch (s[0]) {
		case "rls":
			System.out.println("Archivos Servidor:");
			String[] output = fileSystem.RMI_rls(name, key);

			// Imprimir cada linea
			for (int i = 0; i < output.length; ++i) {
				System.out.println(output[i]);
			}
			break;
		case "lls":
			lls();
			break;
		case "sub":
			File[] files = (new File(".")).listFiles(); // Archivos contenidos en el directorio actual.

			// Verificar que el archivo existe y enviar sus bytes al servidor
			for (int i = 0; i < files.length; ++i) {
				if ((files[i].isFile()) && (files[i].getName().compareTo(s[1]) == 0)) {
					try {
						fileSystem.RMI_sub(Files.readAllBytes(Paths.get(files[i].getAbsolutePath())), name, key, s[1]);
					} catch (IOException e) {
						System.out.println("No se pudo leer correctamente el archivo a subir.");
					}
				}
			}
			break;
		case "baj":
			byte[] fileStream = fileSystem.RMI_baj(s[1], name, key);
			if (fileStream == null) {
				System.out.println("El archivo no existe en el servidor.");
				break;
			}
			try {
				File newFile = new File("./" + s[1]);
				FileOutputStream newFOS = new FileOutputStream(newFile);
				newFile.createNewFile();
				newFOS.write(fileStream);
				newFOS.close();
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
			break;
		case "bor":
			int outcome = fileSystem.RMI_bor(s[1], name, key);
			if (outcome == -1) {
				System.out.println("No se pudo borrar el archivo..");
			} else {
				System.out.println("El archivo fue eliminado exitosamente.");
			}
			break;
		case "info":
			info();
			break;
		case "sal":
			System.out.println("Saliendo del Cliente...");
			System.exit(0);
			break;
		default:
			System.out.println("El comando no es valido.");
			break;
		}
		return 0;
	}

	/**
	 * Comando que muestra información de ayda sobre los posibles comandos que
	 * pueden ejecutarse.
	 */
	private static void info() {
		System.out.println("Los comandos que puede ejecutar son:");
		System.out.println("rls -> muestra la lista de archivos disponibles en servidor centralizado.");
		System.out.println("lls -> muestra la lista de archivos disponibles localmente.");
		System.out
				.println("sub <archivo> -> sube un archivo al servidor remoto (Ej: sub clase.pdf). El archivo especificado \n"
						+ "como parámetro debe estar en la lista de archivos disponibles para el cliente localmente. ");
		System.out.println(
				"baj <archivo> -> baja un archivo desde el servidor remoto (Ej: baj ejemplo.c). El archivo especificado \n"
						+ "debe estar en la lista de archivos disponibles en el servidor centralizado para que el comando funcione\n"
						+ "adecuadamente.");
		System.out.println("bor <archivo> -> borra el archivo en el servidor remoto.");
		System.out.println(
				"sal -> termina la ejecución del programa cliente, notificando este hecho a todos los procesos del sistema\n"
						+ "que lo requieran.");
	}

	/**
	 * Comando que lista los archivos disponibles localmente en el computador.
	 * Muestra los archivos contenidos en el mismo directorio donde se ejecuta
	 * programa.
	 */
	private static void lls() {

		System.out.println("Archivos locales:");

		File[] files = (new File(".")).listFiles(); // Archivos contenidos en el direcotrio actual.

		/* Ciclo que imprime los nombres de los archivos */
		for (int i = 0; i < files.length; i++) {
			if (files[i].isFile()) {
				System.out.println(files[i].getName());
			}
		}

	}

	/* Main Client Implementation */

	public static void main(String[] args) {

		String address = null; // Direccion del servidor al que se va a conectar.
		String port = null; // Puerto por el cual se establecera la conexion.
		String userFile = null; // Nombre del archivo que contiene los usuarios y la clave de existir.
		String cmdFile = null; // Nombre del archivo que indica comandos a ejecutar de existir.

		String user = null; // Nombre de autenticacion
		String key = null; // Clave de autenticacion

		BufferedReader userReader = null; // Lector de userFile
		BufferedReader cmdReader = null; // Lector de cmdFile

		FSIntf fileSystem = null;

		@SuppressWarnings("resource")
		Scanner input = new Scanner(System.in); // Recibe el Input del usuario

		/* Verificacion de argumentos */

		String argument;
		for (int i = 0; i < args.length; i = i + 2) {
			argument = args[i];

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
					System.out.println("El archivo de autenticacion no existe");
					System.exit(-1);
				}
				break;
			case "-m":
				address = args[i + 1];
				break;
			case "-p":
				port = args[i + 1];
				break;
			case "-c":
				cmdFile = args[i + 1];
				try {
					cmdReader = new BufferedReader(new FileReader(cmdFile));
				} catch (IOException e) {
					System.out.println("El archivo de comandos no existe");
					System.exit(-1);
				}
				break;
			}
		}

		// Revisar que se hayan dado todos los argumentos obligatorios
		if (port == null || address == null) {
			System.out.println("No se otorgaron todas las opciones obligatorias.\nUso: "
					+ "java c_rmifs [-f usuarios] -m servidor -p puerto [-c comandos] ");
			System.exit(-1);
		}

		/* Obtener Usuario y Clave */

		// Preguntar por usuario en caso de que no haya sido especificado aun
		String[] authString = null;
		if (userReader == null) {
			System.out.println("No se ha especificado valores de " + "autenticacion.\nIngrese nombre y clave"
					+ "separados por el caracter ':'.");
			authString = input.nextLine().split(":");

			// En caso de que el archivo si ha sido proporcionado
		} else {
			try {
				authString = userReader.readLine().split(":");
				userReader.close();
			} catch (IOException e) {
				System.out.println("Error en la lecutra de usuario.");
				System.exit(-1);
			}
		}

		// Asignar y verificar nombre y clave
		if (authString.length != 2) {
			System.out.println("El ingreso de datos es incorrecto.");
			System.exit(-1);
		} else {
			user = authString[0];
			key = authString[1];
		}

		/* Conexion remota con el servidor */

		try {
			fileSystem = (FSIntf) Naming.lookup("rmi://" + address + ':' + port + "/FSServer");
		} catch (MalformedURLException | RemoteException | NotBoundException e) {
			System.out.println("No se pudo conectar al servidor de archivos.");
			System.exit(-1);
		}

		/* Autenticacion inicial con el servidor */

		try {
			if (fileSystem.RMI_login(user, key) == 1) {
				System.out.println("Autenticacion correcta.");
			} else {
				System.out.println("Autenticacion rechazada.");
				System.exit(-1);
			}
		} catch (RemoteException e1) {
			System.out.println("Error en el login con el servidor.");
			System.exit(-1);
		}

		/* Manejo del archivo de comandos */

		if (cmdReader != null) {
			try {
				String line;
				while ((line = cmdReader.readLine()) != null) {
					commandParser(line.split(" "), user, key, fileSystem);
				}
				cmdReader.close();
			} catch (IOException e) {
				System.out.println("No se pudo leer del archivo de comandos.");
			}
		}

		/* Ciclo del programa */
		System.out.println("Escriba el comando que desea utilizar: (Si necesita ayuda escriba info)");
		while (true) {
			try {
				commandParser(input.nextLine().split(" "), user, key, fileSystem);
			} catch (NoSuchElementException e) {
				System.out.println("No se encontro linea para leer.");
				System.exit(-1);
			} catch (IllegalStateException e) {
				System.out.println("Estado Illegal de Lectura.");
				System.exit(-1);
			} catch (IOException e) {
				System.out.println("IOEXCEPTION");
			}
		}
	}
}