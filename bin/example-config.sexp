(
 ;; :memory 4Gb							;Max memory usable by instances (for automatic memory allocation)
 :instances (
			 (
			  :name "Minecraft"			;Name of the instance
			  :main-jar "minecraft.jar"	;Jar of the instance
			  ;; :memory 2Gb			;Memory to allocate to the instance
			  ;; :extra-args ""			;Extra args to pass to to the JVM
			  :java "java"				;Java binary to use
			  )
			 )
 )
