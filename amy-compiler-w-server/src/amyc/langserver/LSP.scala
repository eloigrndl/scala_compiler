package amyc
package langserver

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import org.eclipse.lsp4j.jsonrpc.Launcher;
import org.eclipse.lsp4j.launch.LSPLauncher;
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.LanguageClient

object LSP {

    def main (args: Array[String]): Unit = {

        //val port: String = "3000"
        try {
            
            LogManager.getLogManager().reset();
            var globalLogger: Logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
            globalLogger.setLevel(Level.OFF);
            //val socket: Socket = new Socket("localhost", Integer.parseInt(port))
            //val in: InputStream = socket.getInputStream()
            //val out: OutputStream = socket.getOutputStream()

            val server: AmyLanguageServer = new AmyLanguageServer
            val launcher: Launcher[LanguageClient] = LSPLauncher.createServerLauncher(server, System.in, System.out);
            val client: LanguageClient = launcher.getRemoteProxy();
            
            server.connect(client);
            launcher.startListening();

        }
        catch {
            case e: Exception => e.printStackTrace()
        }
    }

}
