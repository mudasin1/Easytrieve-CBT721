---
title: CPP099 Jobs Overview
---
# Overview of CPP099 Jobs

CPP099 Jobs consist of a set of standardized routines designed to manage abnormal program termination, commonly referred to as abends. These routines provide a consistent framework for handling errors and ensuring that programs terminate safely and cleanly within the system.

# Purpose and Benefits

The primary purpose of CPP099 Jobs is to maintain uniformity in error handling across multiple programs. By using these jobs, developers can ensure that abend codes are properly assigned, detailed diagnostic information is displayed, and necessary cleanup procedures are executed to prevent resource leaks or inconsistent system states.

# Core Components

At the heart of CPP099 Jobs is the copybook `JXCPP099`, which contains the standardized abend handling code. This copybook is included in programs that require abend management, enabling them to leverage the common routines for error detection, reporting, and cleanup.

# Functionality and Behavior

The routines within CPP099 Jobs monitor for specific abend codes and respond accordingly. They adapt their behavior based on the execution environment, adjusting abend code processing if the program runs under a particular control environment. When an abend occurs, these routines display comprehensive information such as the return code, the paragraph where the abend happened, and any relevant reason codes.

# System-Level Integration

Beyond displaying diagnostic information, CPP099 Jobs invoke system-level abend handling routines to properly terminate the program. Depending on the environment, they call system routines like `CEE3ABD` for controlled environments or `ILBOABN0` otherwise, ensuring that the program exits cleanly and that all necessary cleanup operations are performed.

# How to Use CPP099 Jobs

To incorporate CPP099 Jobs into a program, developers include the copybook `JXCPP099` which embeds the abend handling logic. When an abend is detected, the program sets or verifies the abend codes, outputs detailed messages about the error including the return code and the location of the abend, and then calls the appropriate system routines to finalize the termination process.

# Example Usage

For instance, upon detecting an abend, the code moves the abend code into a return code variable and displays messages indicating the program is abending, along with the abend code and the paragraph where it occurred. Subsequently, it calls system routines such as `CEE3ABD` if running under a specific control environment or `ILBOABN0` otherwise to handle the abend cleanup and termination.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBRWFzeXRyaWV2ZS1DQlQ3MjElM0ElM0FtdWRhc2luMQ==" repo-name="Easytrieve-CBT721"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
