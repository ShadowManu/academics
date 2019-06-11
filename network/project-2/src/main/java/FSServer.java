import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Scanner;


public class FSServer
	extends UnicastRemoteObject
	implements FSIntf {

	private static final long serialVersionUID = 1L;
	private ASIntf authSystem;
	private static MyList<String> log;
	private static final int logSize = 20;
	
	private MyList<String[]> files;			// Lista de Archivos con su propietario
	
	public FSServer(String authAddress, String authPort) throws RemoteException {
		super();
		
		// Conectar al servidor de autenticacion
		try {
			authSystem = (ASIntf) Naming.lookup("rmi://" + authAddress + ':'
					+ authPort + "/ASServer");
		} catch (MalformedURLException | RemoteException | NotBoundException e) {
			System.out.println("No se pudo conectar al servidor de autenticacion.");
			// #! System.exit(-1);
		}
		
		// Iniciar la lista de archivos y el log
		files = new MyList<String[]>();
		log = new MyList<String>();
	}
	
	/* Internal Server Procedures */
	
	private boolean isClient(String user, String key) throws RemoteException {
		return authSystem.isClient(user, key);
	}
	
	private void addLog(String entry) {
		log.add(entry);
		if (log.getSize() > logSize) {
			log.remove(0);
		}
	}
	
	private static void printLog() {
		ListIterator<String> listIter = log.iterator();
		
		System.out.println("Los ultimos " + logSize + " comandos "
				+ "introducidos son:");
		
		while (listIter.hasNext()) {
			System.out.println(listIter.next());
		}
	}
	
	/* Remote Services */
	
	public int RMI_login(String name, String key) throws RemoteException {
		if (isClient(name, key)) {
			return 1;
		}
		return 0;
	}
	
	public String[] RMI_rls(String name, String key)
			throws RemoteException {
		
		addLog(name + " : rls");
		String[] out = new String[files.getSize()];
		ListIterator<String[]> listIter = files.iterator();
		int i = 0;
	
		while (listIter.hasNext()) {
			out[i] = listIter.next()[0];
			++i;
		}
		
		return out;
	}
	
	public int RMI_sub(byte[] file, String name, String key, String filename)
			throws RemoteException {
		addLog(name + " : sub " + filename);
		try {
			File newFile = new File("./"+filename);
			FileOutputStream newFOS = new FileOutputStream(newFile);
			newFile.createNewFile();
			newFOS.write(file);
			newFOS.close();
	
			String[] ufile = new String[2];
			ufile[0] = filename;
			ufile[1] = name;
			files.add(ufile);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return 0;
	}
	
	public byte[] RMI_baj(String filename, String name, String key)
			 throws RemoteException {
		
		addLog(name + " : baj " + filename);
		String[] dfile = new String[2];
		dfile[0] = filename;
		dfile[1] = name;
		
		// Verifica que el archivo este en la lista de archivos validos en el servidor
		if (files.contains(dfile)) {
			
			// Verifica que el archivo exista en el sistema
			File[] files = (new File(".")).listFiles();
			for (int i = 0; i < files.length; i++) {
				
				if ( (files[i].isFile()) && (files[i].getName().compareTo(filename) == 0) ) {
					try {
						return Files.readAllBytes(Paths.get(files[i].getAbsolutePath()));
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
				
			}
			
		}
		return null;
	}
	
	public int RMI_bor(String filename, String name, String key)
			throws RemoteException {
		
		addLog(name + " : bor " + filename);
		String[] efile = new String[2];
		efile[0] = filename;
		efile[1] = name;
		
		// Verifica que el archivo se encuentre en la lista del servidor
		if (files.contains(efile)){
			
			File[] lfiles = (new File(".")).listFiles(); // Archivos contenidos en el direcotrio actual.
			
			/* Ciclo que verifica que el archivo existe*/
			for (int i = 0; i < lfiles.length; i++){
				
				if ( (lfiles[i].isFile()) && (lfiles[i].getName().compareTo(filename) == 0) ) {
					lfiles[i].delete();
					files.remove(efile);
					return 0;
				}
				
			}

		}
		return -1;
	}
	
	/* Main Server Implementation */
	
	public static void main (String[] args) {

		String localPort = null;	// Puerto local del sv de archivos
		String authAddress = null;	// Direccion del sv de autenticacion
		String authPort = null;		// Puerto del sv de autenticacion
		
		FSServer fileSystem = null;	// Objeto servidor remoto
		
		/* Verificacion de argumentos */
		
		for (int i = 0 ; i < args.length; i=i+2){
			String argument = args[i];
			
			if (i+1 > args.length){
				System.out.println("Los argumentos estan incompletos.");
				System.exit(-1);
			}
			
			switch (argument) {
			case "-l":
				localPort = args[i+1];
				break;
			case "-h":
				authAddress = args[i+1];
				break;
			case "-r":
				authPort = args[i+1];
				break;
			default:
				System.out.println("Se ha dado una opcion invalida.\nUso: "
						+ "java s_rmifs -l puertolocal -h host -r puerto.");
				System.exit(-1);
				break;
			}
		}
		
		// Revisar que se hayan dado todos los argumentos obligatorios
		if  (localPort == null || authAddress == null || authPort == null) {
			System.out.println("No se otorgaron todas las opciones.\nUso: "
					+ "java s_rmifs -l puertolocal -h host -r puerto.");
			System.exit(-1);
		}
		
		/* Configuracion de servidor */
		
		// RMI Registry Run
		try {
            LocateRegistry.createRegistry(Integer.parseInt(localPort));
            System.out.println("RMI Registry creado.");
        } catch (RemoteException e) {
            System.out.println("RMI Registry ya existe.");
        }
		
		// Generacion de Servidor
		try {
			fileSystem = new FSServer(authAddress, authPort);
		} catch (RemoteException e) {
			System.out.println("El objeto servidor remoto no pudo ser creado.");
			System.exit(-1);
		}
		
		// Anclaje de objeto a direccion
		try {
			Naming.rebind("rmi://localhost:" + localPort + "/FSServer", fileSystem);
			System.out.println("Servidor de Archivos anclado.");
		} catch (RemoteException e) {
			System.out.println("El Servidor de Archivos no pudo ser anclado.");
			System.exit(-1);
		} catch (MalformedURLException e) {
			System.out.println("La direccion del servidor ha sido malformada.");
			System.exit(-1);
		}
		
		// Ciclo del Servidor
		@SuppressWarnings("resource")
		Scanner input = new Scanner(System.in);	// Recibe el Input del usuario
		String StrInput;
		
		System.out.println("Escriba el comando que desea utilizar:");
		while (true) {
			StrInput = input.nextLine();
			if (StrInput.compareTo("log")==0){
				System.out.println();
				printLog();
			} else if ((StrInput.compareTo("sal")==0)) {
				System.out.println("Saliendo...");
				System.exit(0);
			} else {
				System.out.println("El comando que ingreso no es valido.");
			}
		}
	}
}
