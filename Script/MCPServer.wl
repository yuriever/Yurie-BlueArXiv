(* ::Package:: *)

<<Yurie`BlueArXiv`Info`


PacletInstall["Wolfram/AgentTools"]
Needs["Wolfram`AgentTools`"]


Keys[$SupportedMCPClients]


InstallMCPServer[
    {"ClaudeCode", $thisPacletDir},
    "WolframLanguage",
    "MCPServerName" -> "WolframLanguage"
]


InstallMCPServer[
    {"Codex", $thisPacletDir},
    "WolframLanguage",
    "MCPServerName" -> "WolframLanguage"
]

