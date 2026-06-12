import { workspace, ExtensionContext, window } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';
import * as fs from 'fs';

let client: LanguageClient;

function resolveLspPath(configured: string): string {
    const folder = workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';
    return configured
        .replace(/\$\{workspaceFolder\}/g, folder)
        .replace(/\$\{workspaceRoot\}/g, folder);
}

export function activate(context: ExtensionContext) {
    const config = workspace.getConfiguration('ion');
    const configured = config.get<string>('lspPath') || 'ion-lsp';
    const serverPath = resolveLspPath(configured);

    if (!fs.existsSync(serverPath)) {
        void window.showWarningMessage(
            `Ion LSP not found at: ${serverPath}. Build with: cargo build --release --bin ion-lsp`
        );
    }

    const serverOptions: ServerOptions = {
        command: serverPath,
        args: [],
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'ion' }],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/*.ion'),
        },
    };

    client = new LanguageClient(
        'ionLanguageServer',
        'Ion Language Server',
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
