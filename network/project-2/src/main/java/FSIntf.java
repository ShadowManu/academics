import java.rmi.Remote;
import java.rmi.RemoteException;

public interface FSIntf extends Remote {
	
	public int RMI_login(String name, String key) throws RemoteException;
	
	public String[] RMI_rls(String name, String key) throws RemoteException;
	
	public int RMI_sub(byte[] file, String name, String key, String filename) throws RemoteException;
	
	public byte[] RMI_baj(String filename, String name, String key) throws RemoteException;
	
	public int RMI_bor(String filename, String name, String key) throws RemoteException;
	
}
