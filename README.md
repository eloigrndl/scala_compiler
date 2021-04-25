# Amy Compiler

Third year project @ EPFL ([CS_320 course](https://edu.epfl.ch/coursebook/en/computer-language-processing-CS-320))<br />
Eloi Garandel / Raoul Gerber / Szabina Horváth-Mikulas / Erik Wengle

The main goal of this course was to implement a programming language based on Scala : the [Amy language](http://lara.epfl.ch/~gschmid/clp20/amy-spec.pdf).
The first step was to create a compiler for this language and then create an extension for the compiler. The compiler is in the amy-compiler-w-server folder and the extension is described below. 

The report pdf file gives a brief explanantion of the overall project and the extension we made.


## Amy LSP Extension

It consists of two main parts: the LSP client (in amy-client folder) for VS Code and the LSP server (in amy-compiler-w-server folder) for Amy. You can also find a test folder (amy-test) to test the extension.

A) LSP client
Instructions to launch the extension:

1. Open the amy-client folder in a separate VS Code window.
2. Open the terminal in the current folder.
2. Run the command – npm install - to install the nec. npm packages.
4. Run the command - npm run compile – to compile TS to JS.
5. Use the Run extension command (F5) in VS Code to run the extension in a new Extension Development Host window (Run icon on the left, icon on top).
6. See --> B) Test Extension

Info: the extension.ts will activate the server by running the jar of the Amy compiler that has the server integrated in it. The current path to the jar is cs320-group11/amy-client/launcher/amy-server.jar which is specified in extension.ts.
The jar is automatically updated whenever the Amy Scala compiler project is “assembled” by running the – assembly – command in sbt. You can see the reference in cs320-group11/build.sbt.

B) Test Extension
0. Precondition: Your URI should start with /Users/ on top level. (Our way to trim the file URI provided to a usable version relies on it having /Users/ at a top-level. This is fine for default Windows and MacOS, but probably not for Linux distributions.)
1. In the Extension Development Host window open the amy-test folder.
2. The amy-test folder contains a sub-folder called library, which is the standard library of Amy. Do not delete or move the folder. Include it to the root folder of each project.
3. Open fact.amy and wait around 1 min till the extension gets activated.
4. In the OUTPUT window you can see error msg-s sending by the Amy compiler.

Info: In fact.amy you can see the implemented coloring. You can check the go to definition feature as well.
In hello.amy you can request go to definition on the Std module name that will redirect you to library/Std.amy.
Auto-completion trials are added for sayHello and match.
 
C) LSP server
We took the state of branch Lab05 as a reference for our compiler.
The LSP server is integrated into the compiler under the langserver folder. In this way it has access to all packages of the Amy compiler.
Instructions to create a FAT jar from the Amy compiler with integrated server:

1. Open the cs320-group11/amy-compiler-w-server folder in sbt
2. Compile the project by – compile – command in sbt
3. Run the – assembly – command to create a jar

Info: the jar will be created in the LSP client launcher folder - amy-client/launcher/amy-server.jar. Have a look at the build.sbt file [here](https://lara.epfl.ch/w/cc20/labs_06).


