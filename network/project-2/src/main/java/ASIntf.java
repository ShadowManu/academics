import java.rmi.Remote;
import java.rmi.RemoteException;

public interface ASIntf extends Remote {
	
	public boolean isClient(String name, String key)
			throws RemoteException;

}
