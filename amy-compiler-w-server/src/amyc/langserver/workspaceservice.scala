package amyc
package langserver

import java.util
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.DidChangeWorkspaceFoldersParams
import org.eclipse.lsp4j.ExecuteCommandParams
import org.eclipse.lsp4j.SymbolInformation
import org.eclipse.lsp4j.WorkspaceSymbolParams
import org.eclipse.lsp4j.services.WorkspaceService

class AmyWorkspaceService extends WorkspaceService {
	
	override def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] =
		return null
    
	override def symbol(params: WorkspaceSymbolParams): CompletableFuture[util.List[_ <: SymbolInformation]] =
		return null

	override def didChangeConfiguration(params: DidChangeConfigurationParams) : Unit = {
    }

	override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams) : Unit = {
    }
    
	override def didChangeWorkspaceFolders(params: DidChangeWorkspaceFoldersParams): Unit = {
    }
	
}