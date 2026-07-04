(* ::Package:: *)

<<Yurie`BlueArXiv`Info`


PacletInstall["Wolfram/AgentTools"]

Needs["Wolfram`AgentTools`"]

Keys[$SupportedMCPClients]


InstallMCPServer[{"Codex",$thisPacletDir},"WolframLanguage"]

InstallMCPServer[{"ClaudeCode",$thisPacletDir},"WolframLanguage"]
