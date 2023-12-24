# TMS-WEB-Core-ActorInfo
This repository contains the complete Delphi source code for an example of using a [TMS XData](https://www.tmssoftware.com/site/xdata.asp) server and a [TMS WEB Core](https://www.tmssoftware.com/site/tmswebcore.asp) application to create a simple interface to see all the actors that share a given birthday, along with information about their roles, popularity, and so on. This is a companion to a [TMS Blog post](https://www.tmssoftware.com/site/blog.asp?post=949) first published on June 16, 2022. The data for this project comes from both [WikiData](https://www.wikidata.org/wiki/Wikidata:Main_Page) and [The Movie Database](https://www.themoviedb.org). This project has been further developed into the [Actorious](https://www.actorious.com/) project, with its own [Client](https://github.com/500Foods/ActoriousClient) and [Server](https://github.com/500Foods/ActoriousServer) repositories.  

![ActorInfo](https://user-images.githubusercontent.com/41052272/173462502-bb6579c0-76ee-4e28-b172-6dfad7254d2e.gif)

## More Information
**ActorInfo** contains the TMS XData server code. This is used to query WikiData and TMDb.org for information about birthdays and actor roles, respectively, and includes examples of how to cache requests, parse reasonably sane JSON data, and various other little tips and tricks. Note that ActorInfo also requires the latest OpenSSL libraries. Please refer to the source code for more information.

**TabulatorDemo** contains the TMS WEB Core client application that connects to ActorInfo to get its data. It also relies on FlatPickr to provide the date-time UI and Tabulator for the grid components. Only the project files are included here, so be mindful of those dependencies, and also that Bootstrap 5 is used.

## Repository Information
[![Count Lines of Code](https://github.com/500Foods/TMS-WEB-Core-ActorInfo/actions/workflows/main.yml/badge.svg)](https://github.com/500Foods/TMS-WEB-Core-ActorInfo/actions/workflows/main.yml)
<!--CLOC-START -->
```
Last Updated at 2023-11-27 02:18:21 UTC
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Pascal                           7            163            288            495
Delphi Form                      3              1             13            241
HTML                             2              2              2             36
YAML                             2              8             12             33
Markdown                         1              5              2             24
-------------------------------------------------------------------------------
SUM:                            15            179            317            829
-------------------------------------------------------------------------------
```
<!--CLOC-END-->

## Sponsor / Donate / Support
If you find this work interesting, helpful, or valuable, or that it has saved you time, money, or both, please consider directly supporting these efforts financially via [GitHub Sponsors](https://github.com/sponsors/500Foods) or donating via [Buy Me a Pizza](https://www.buymeacoffee.com/andrewsimard500). Also, check out these other [GitHub Repositories](https://github.com/500Foods?tab=repositories&q=&sort=stargazers) that may interest you.

## More TMS WEB Core and TMS XData Content
If you're interested in other TMS WEB Core and TMS XData content, follow along on 𝕏 at [@WebCoreAndMore](https://x.com/WebCoreAndMore), join our 𝕏 [Web Core and More Community](https://twitter.com/i/communities/1683267402384183296), or check out the [TMS Software Blog](https://www.tmssoftware.com/site/blog.asp).
