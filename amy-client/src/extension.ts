import * as path from 'path';
import * as vscode from 'vscode';

// Import the language client, language client options and server options from VSCode language client.
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient';
import * as fs from "fs"

// Name of the launcher class inside the .jar which is the entry point to the server.
const main: string = 'amyc.Main';

export function activate(context: vscode.ExtensionContext) {
	
	console.log('Congratulations, your extension "Amy LSP" is now active!');

	// Java execution path
	let excecutable: string = 'java';

	// Path to the .jar of the server launcher
	let classPath = path.join(__dirname, '..', 'launcher', 'amy-server.jar');
	// Argument to be passed when executing the java command
	// LSP flag to run the compiler in server mode
	const args: string[] = ['-cp', classPath, main, '--LSP'];

	// Options to control the language server.
	let serverOptions: ServerOptions = {
		command: excecutable,
		args: [...args],
		options: {}
	};

	// Options to control the language client.
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents.
		documentSelector: [{ scheme: 'file', language: 'amy' }]
	};

	// Create the language client and start the client.
	let disposable = new LanguageClient('amyLSP', 'Amy Language Server', serverOptions, clientOptions).start();

	let logFile = path.join(__dirname, '..', 'log', 'tryLog.log');
	let logStream = fs.createWriteStream(logFile, { flags: 'w' });

	process.stdout.pipe(logStream);
	process.stderr.pipe(logStream);

	console.log(`Storing log in '${logFile}'`);

	// Disposables to remove on deactivation.
	context.subscriptions.push(disposable);
}

// This method is called when your extension is deactivated.
export function deactivate() { 
	console.log('Your extension "Amy LSP" is now deactivated!');
}
