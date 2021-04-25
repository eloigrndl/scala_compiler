package amyc
package langserver

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.CompletionOptions
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.InitializeResult
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.TextDocumentSyncKind
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService

// Implement the language server by implementing the interface LanguageServer
// Launch it and connect it to a client
class AmyLanguageServer extends LanguageServer with LanguageClientAware {
    
    // To make our server client aware it implements the LanguageClientAware interface
    private var client: Option[LanguageClient] = None
    // 2 services that our server provides
    private val textDocumentService: TextDocumentService = new AmyTextDocumentService
    private val workspaceService: WorkspaceService = new AmyWorkspaceService

    /**
	 * The initialize request is sent as the first request from the client to
	 * the server.
	 * 
	 * If the server receives request or notification before the initialize request it should act as follows:
	 * 	- for a request the respond should be errored with code: -32001. The message can be picked by the server.
	 *  - notifications should be dropped, except for the exit notification. This will allow the exit a server without an initialize request.
	 *  
	 * Until the server has responded to the initialize request with an InitializeResult 
	 * the client must not sent any additional requests or notifications to the server.
	 * 
	 * During the initialize request the server is allowed to sent the notifications window/showMessage, 
	 * window/logMessage and telemetry/event as well as the window/showMessageRequest request to the client.
	 */ 
    override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
        // Set server capabilities
        var capabilities: ServerCapabilities = new ServerCapabilities()
        capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
        // Adding completion capability (only supported capability to start with...)
        capabilities.setCompletionProvider(new CompletionOptions)
        capabilities.setDefinitionProvider(true)

        // Return the InitializeResult
        CompletableFuture.supplyAsync(() => new InitializeResult(capabilities))
    }

    /**
	 * The shutdown request is sent from the client to the server. It asks the
	 * server to shutdown, but to not exit (otherwise the response might not be
	 * delivered correctly to the client). There is a separate exit notification
	 * that asks the server to exit.
	 */
    override def shutdown(): CompletableFuture[Object] = {
        CompletableFuture.completedFuture(None)
    }

    /**
	 * A notification to ask the server to exit its process.
	 */
    override def exit(): Unit = {
        // Kill the LS on exit request from client.
        System.exit(1)
    }

    /**
	 * Provides access to the textDocument services.
	 */
    override def getTextDocumentService(): TextDocumentService = {
        // Return the endpoint for language features.
        return this.textDocumentService
    }
    
    /**
	 * Provides access to the workspace services.
	 */
    override def getWorkspaceService(): WorkspaceService = {
        // Return the endpoint for workspace functionality.
        return this.workspaceService
    }

    /*
    * Method in LanguageClientAware
    */
    override def connect(client: LanguageClient): Unit = {
        this.client = Option(client)
    }
}