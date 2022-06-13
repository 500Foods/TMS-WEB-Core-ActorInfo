# TMS-WEB-Core-ActorInfo

This repository contains the complete Delphi source code for an example of using a TMS XData server and a TMS WEB Core application to create a simple interface to see all the actors that share a given birthday, along with information about their roles, popularity, and so on. This is a companion to a TMS Blog post first published on June 16, 2022.

![ActorInfo](https://user-images.githubusercontent.com/41052272/173462502-bb6579c0-76ee-4e28-b172-6dfad7254d2e.gif)

## Contents

**ActorInfo** contains the TMS XData server code. This is used to query WikiData and TMDb.org for information about birthdays and actor roles, respectively, and includes examples of how to cache requests, parse reasonably sane JSON data, and various other little tips and tricks.

**TabulatorDemo** contains the TMS WEB Core client application that connects to ActorInfo to get its data.  It also relies on FlatPickr to provide the date-time UI and Tabulator for the grid components.  

&nbsp;

If you find this repository useful to your work, or that it has saved you time or effort when it comes to crafting a better TMS WEB Core project, please consider supporting my efforts via "[Buy Me A :pizza:](https://www.buymeacoffee.com/andrewsimard500)". You may also be interested in my other projects/repositories here on GitHub:
- [TMS Blog To-Do List](https://github.com/users/500Foods/projects/1)
- [TMS WEB Core JSON Primer](https://github.com/500Foods/TMS-WEB-Core-JSON-Primer)
- [JSExtend](https://github.com/500Foods/TMS-WEB-Core-JSExtend) 

&nbsp;  
Enjoy!

Andrew Simard, Founder,  
500 Foods Corporation.
