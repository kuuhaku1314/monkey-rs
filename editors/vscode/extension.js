const vscode = require("vscode");
const childProcess = require("child_process");
const fs = require("fs");
const path = require("path");

let diagnostics;
let output;
const symbolCache = new Map();
const pendingDiagnostics = new Map();

function activate(context) {
  diagnostics = vscode.languages.createDiagnosticCollection("monkey");
  output = vscode.window.createOutputChannel("Monkey");
  context.subscriptions.push(diagnostics, output);

  const selector = { language: "monkey" };

  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument(scheduleDiagnostics),
    vscode.workspace.onDidChangeTextDocument((event) => scheduleDiagnostics(event.document)),
    vscode.workspace.onDidSaveTextDocument(scheduleDiagnostics),
    vscode.workspace.onDidCloseTextDocument((document) => {
      diagnostics.delete(document.uri);
      symbolCache.delete(document.uri.toString());
    }),
    vscode.languages.registerDocumentFormattingEditProvider(selector, {
      provideDocumentFormattingEdits(document) {
        return provideFormatting(document);
      },
    }),
    vscode.languages.registerCompletionItemProvider(selector, {
      provideCompletionItems(document, position) {
        return provideCompletions(document, position);
      },
    }, ".", "\"", "/"),
    vscode.languages.registerHoverProvider(selector, {
      provideHover(document, position) {
        return provideHoverInfo(document, position);
      },
    }),
    vscode.languages.registerDefinitionProvider(selector, {
      provideDefinition(document, position) {
        return provideDefinitionLocation(document, position);
      },
    }),
    vscode.languages.registerReferenceProvider(selector, {
      provideReferences(document, position, context) {
        return provideReferenceLocations(document, position, context);
      },
    }),
    vscode.commands.registerCommand("monkey.runFile", runCurrentFile)
  );

  for (const document of vscode.workspace.textDocuments) {
    if (document.languageId === "monkey") {
      scheduleDiagnostics(document);
    }
  }
}

function deactivate() {}

function scheduleDiagnostics(document) {
  if (document.languageId !== "monkey") {
    return;
  }
  const key = document.uri.toString();
  const pending = pendingDiagnostics.get(key);
  if (pending) {
    clearTimeout(pending);
  }
  pendingDiagnostics.set(
    key,
    setTimeout(async () => {
      pendingDiagnostics.delete(key);
      await refreshDiagnostics(document);
      await refreshSymbols(document);
    }, 250)
  );
}

async function refreshDiagnostics(document) {
  try {
    const result = await runMonkey(document, ["--diagnose-stdin", documentPath(document)], document.getText());
    const payload = JSON.parse(result.stdout || "{\"diagnostics\":[]}");
    const items = (payload.diagnostics || []).map((diagnostic) => {
      const range = toRange(diagnostic.range);
      const item = new vscode.Diagnostic(
        range,
        diagnostic.message || "Monkey error",
        vscode.DiagnosticSeverity.Error
      );
      item.source = diagnostic.source || "monkey";
      return item;
    });
    diagnostics.set(document.uri, items);
  } catch (error) {
    diagnostics.set(document.uri, [
      new vscode.Diagnostic(
        new vscode.Range(0, 0, 0, 1),
        `Monkey diagnostics failed: ${error.message}`,
        vscode.DiagnosticSeverity.Warning
      ),
    ]);
  }
}

async function refreshSymbols(document) {
  try {
    const result = await runMonkey(document, ["--symbols-stdin", documentPath(document)], document.getText());
    const payload = JSON.parse(result.stdout || "{\"completions\":[]}");
    symbolCache.set(document.uri.toString(), payload.completions || []);
  } catch (_) {
    symbolCache.set(document.uri.toString(), []);
  }
}

async function provideFormatting(document) {
  const result = await runMonkey(document, ["--format-stdin", documentPath(document)], document.getText());
  const fullRange = new vscode.Range(
    document.positionAt(0),
    document.positionAt(document.getText().length)
  );
  return [vscode.TextEdit.replace(fullRange, trimSingleTrailingNewline(result.stdout))];
}

async function provideCompletions(document, position) {
  const rustCompletions = await provideRustCompletions(document, position);
  if (rustCompletions) {
    return rustCompletions;
  }

  let symbols = symbolCache.get(document.uri.toString());
  if (!symbols) {
    await refreshSymbols(document);
    symbols = symbolCache.get(document.uri.toString()) || [];
  }
  return symbols.map((symbol) => {
    const item = new vscode.CompletionItem(symbol.label, completionKind(symbol.kind));
    item.detail = symbol.detail;
    return item;
  });
}

async function provideRustCompletions(document, position) {
  try {
    const result = await runMonkey(
      document,
      ["--complete-stdin", documentPath(document), String(position.line), String(position.character)],
      document.getText()
    );
    const payload = JSON.parse(result.stdout || "{\"completions\":[]}");
    return (payload.completions || []).map((symbol) => {
      const item = new vscode.CompletionItem(symbol.label, completionKind(symbol.kind));
      item.detail = symbol.detail;
      return item;
    });
  } catch (error) {
    outputLine(`completion failed: ${error.message}`);
    return undefined;
  }
}

async function provideHoverInfo(document, position) {
  let symbols = symbolCache.get(document.uri.toString());
  if (!symbols) {
    await refreshSymbols(document);
    symbols = symbolCache.get(document.uri.toString()) || [];
  }
  const range = document.getWordRangeAtPosition(position, /[A-Za-z_][A-Za-z0-9_]*/);
  if (!range) {
    return undefined;
  }
  const word = document.getText(range);
  const member = memberAtRange(document, range);
  if (member) {
    const completions = await rustCompletionsAt(document, new vscode.Position(range.start.line, member.memberStart));
    const symbol = completions.find((candidate) => candidate.label === word);
    if (symbol) {
      return new vscode.Hover(new vscode.MarkdownString(`\`${symbol.detail}\``), range);
    }
  }
  const symbol = symbols.find((candidate) => candidate.label === word);
  if (!symbol) {
    return undefined;
  }
  return new vscode.Hover(new vscode.MarkdownString(`\`${symbol.detail}\``), range);
}

async function provideDefinitionLocation(document, position) {
  try {
    const result = await runMonkey(
      document,
      ["--definition-stdin", documentPath(document), String(position.line), String(position.character)],
      document.getText()
    );
    const payload = JSON.parse(result.stdout || "{\"definition\":null}");
    if (!payload.definition) {
      return undefined;
    }
    const uri = payload.definition.path
      ? vscode.Uri.file(resolveDefinitionPath(document, payload.definition.path))
      : document.uri;
    return new vscode.Location(uri, toRange(payload.definition.range));
  } catch (error) {
    outputLine(`definition failed: ${error.message}`);
    return undefined;
  }
}

async function provideReferenceLocations(document, position, context) {
  try {
    const result = await runMonkey(
      document,
      [
        "--references-stdin",
        documentPath(document),
        String(position.line),
        String(position.character),
        context && context.includeDeclaration ? "true" : "false",
      ],
      document.getText()
    );
    const payload = JSON.parse(result.stdout || "{\"references\":[]}");
    return (payload.references || []).map((reference) => {
      const uri = reference.path
        ? vscode.Uri.file(resolveDefinitionPath(document, reference.path))
        : document.uri;
      return new vscode.Location(uri, toRange(reference.range));
    });
  } catch (error) {
    outputLine(`references failed: ${error.message}`);
    return [];
  }
}

function resolveDefinitionPath(document, targetPath) {
  if (path.isAbsolute(targetPath)) {
    return targetPath;
  }
  return path.resolve(path.dirname(documentPath(document)), targetPath);
}

function memberAtRange(document, range) {
  const line = document.lineAt(range.start.line).text;
  const beforeWord = line.slice(0, range.start.character);
  const match = beforeWord.match(/([A-Za-z_][A-Za-z0-9_]*)\.$/);
  if (!match) {
    return undefined;
  }
  return { receiver: match[1], memberStart: range.start.character };
}

async function rustCompletionsAt(document, position) {
  try {
    const result = await runMonkey(
      document,
      ["--complete-stdin", documentPath(document), String(position.line), String(position.character)],
      document.getText()
    );
    const payload = JSON.parse(result.stdout || "{\"completions\":[]}");
    return payload.completions || [];
  } catch (error) {
    outputLine(`hover completion failed: ${error.message}`);
    return [];
  }
}

function runCurrentFile() {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== "monkey") {
    vscode.window.showWarningMessage("Open a Monkey file first.");
    return;
  }
  if (editor.document.isUntitled) {
    vscode.window.showWarningMessage("Save the Monkey file before running it.");
    return;
  }
  const config = vscode.workspace.getConfiguration("monkey", editor.document.uri);
  const terminal = vscode.window.createTerminal({
    name: "Monkey",
    cwd: resolveCargoRoot(editor.document, config),
  });
  const command = shellCommandForRun(editor.document);
  terminal.show();
  terminal.sendText(command);
}

function runMonkey(document, args, input) {
  const config = vscode.workspace.getConfiguration("monkey", document.uri);
  const executablePath = config.get("executablePath");
  const cwd = resolveCargoRoot(document, config);
  const command = executablePath || "cargo";
  const commandArgs = executablePath ? args : ["run", "--quiet", "--", ...args];

  if (executablePath && !fs.existsSync(resolveConfiguredExecutable(executablePath, cwd))) {
    return Promise.reject(new Error(`monkey.executablePath not found: ${executablePath}`));
  }

  return new Promise((resolve, reject) => {
    const child = childProcess.spawn(command, commandArgs, {
      cwd,
      shell: process.platform === "win32",
    });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk) => {
      stdout += chunk.toString();
    });
    child.stderr.on("data", (chunk) => {
      stderr += chunk.toString();
    });
    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) {
        resolve({ stdout, stderr });
      } else {
        reject(new Error(stderr || stdout || `monkey command failed with exit code ${code}`));
      }
    });
    child.stdin.end(input);
  });
}

function shellCommandForRun(document) {
  const config = vscode.workspace.getConfiguration("monkey", document.uri);
  const executablePath = config.get("executablePath");
  const file = document.uri.fsPath;
  if (executablePath) {
    return `${quoteShell(executablePath)} ${quoteShell(file)}`;
  }
  return `cargo run --quiet -- ${quoteShell(file)}`;
}

function resolveConfiguredExecutable(executablePath, cwd) {
  if (path.isAbsolute(executablePath)) {
    return executablePath;
  }
  return path.join(cwd, executablePath);
}

function resolveCargoRoot(document, config) {
  const configured = config.get("cargoRoot");
  if (configured) {
    return configured;
  }
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
  const start = workspaceFolder ? workspaceFolder.uri.fsPath : path.dirname(document.uri.fsPath);
  return findCargoRoot(start) || start;
}

function findCargoRoot(start) {
  let current = start;
  while (current && current !== path.dirname(current)) {
    if (fs.existsSync(path.join(current, "Cargo.toml"))) {
      return current;
    }
    current = path.dirname(current);
  }
  return undefined;
}

function documentPath(document) {
  return document.isUntitled ? "untitled.monkey" : document.uri.fsPath;
}

function toRange(range) {
  if (!range) {
    return new vscode.Range(0, 0, 0, 1);
  }
  return new vscode.Range(
    range.start.line,
    range.start.character,
    range.end.line,
    Math.max(range.end.character, range.start.character + 1)
  );
}

function completionKind(kind) {
  switch (kind) {
    case "function":
    case "builtin":
      return vscode.CompletionItemKind.Function;
    case "struct":
      return vscode.CompletionItemKind.Class;
    case "module":
      return vscode.CompletionItemKind.Module;
    case "field":
      return vscode.CompletionItemKind.Field;
    case "file":
      return vscode.CompletionItemKind.File;
    case "folder":
      return vscode.CompletionItemKind.Folder;
    case "keyword":
      return vscode.CompletionItemKind.Keyword;
    default:
      return vscode.CompletionItemKind.Variable;
  }
}

function trimSingleTrailingNewline(value) {
  return value.endsWith("\n") ? value.slice(0, -1) : value;
}

function outputLine(message) {
  if (output) {
    output.appendLine(message);
  }
}

function quoteShell(value) {
  if (process.platform === "win32") {
    return `"${value.replace(/"/g, '\\"')}"`;
  }
  return `'${value.replace(/'/g, "'\\''")}'`;
}

module.exports = {
  activate,
  deactivate,
};
