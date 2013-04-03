rehc
====

rehc is an application that provides:

	-monitor for apps on remote host ().
	-statistics about cpu usage, memory consumption and disk space.
	-API web for view info about remote hosts.
	-Real time statistics via websockets.
	-Notifications by email when apps are down or critical usage (cpu, memory and disk).
	-Backup your remote mnesia nodes
	
	
downloading and installing
====

Download rehc from the git repo

		git clone https://github.com/jorgegarrido/rehc
		
Let's into rehc directory

		cd rehc
		
Now like unix/linux applications, install normally by:

		./configure && make && make install
	
The REHC Fast Track
====

Here you can find info about configuration and how to start with rehc: [The REHC Fast Track](https://github.com/jorgegarrido/rehc/wiki/The-REHC-Fast-Track)

Web Based API
====

rehc provides a based web API to retrive info about remote hosts, visit: [Web Based API](https://github.com/jorgegarrido/rehc/wiki/Web-Based-API)


Author
====

Jorge Garrido <zgbjgg@gmail.com>

LICENSE	
====

THIS SOFTWARE IS LICENSED UNDER BSD LICENSE. see LICENSE.txt for more info

