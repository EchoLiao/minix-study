/* Description of entry in partition table.  */
#ifndef _PARTITION_H
#define _PARTITION_H

struct part_entry {
  unsigned char bootind;	/* boot indicator 0/ACTIVE_FLAG	 */
  unsigned char start_head;	/* head value for first sector	 */
  unsigned char start_sec;	/* sector value + cyl bits for first sector */
  unsigned char start_cyl;	/* track value for first sector	 */
  unsigned char sysind;		/* system indicator		 */
  unsigned char last_head;	/* head value for last sector	 */
  unsigned char last_sec;	/* sector value + cyl bits for last sector */
  unsigned char last_cyl;	/* track value for last sector	 */
  unsigned long lowsec;		/* logical first sector		 */
  unsigned long size;		/* size of partition in sectors	 */
};

#define ACTIVE_FLAG	0x80	/* value for active in bootind field (hd0) */
#define NR_PARTITIONS	4	/* number of entries in partition table */
#define	PART_TABLE_OFF	0x1BE /* offset of partition table in boot sector. 见Figure 1 */

/* Partition types. */
#define NO_PART		0x00	/* unused entry */
#define MINIX_PART	0x81	/* Minix partition type */

#endif /* _PARTITION_H */

 /* 0000   |------------------------------------------------|
		   |                                                |
		   |                                                |
		   |                Master Boot Record	            |
		   |                                                |
		   |                                                |
		   |                主引导记录(446字节)		        |
		   |                                                |
		   |                                                |
		   |                                                |
	01BD   |                                                |
    01BE 446------------------------------------------------|
		   |                                                |
	01CD   |              分区信息   1(16字节)              |
	01CE   |------------------------------------------------|
		   |                                                |
	01DD   |              分区信息   2(16字节)              |
	01DE   |------------------------------------------------|
		   |                                                |
	01ED   |              分区信息   3(16字节)              |
	01EE   |------------------------------------------------|
		   |                                                |
	01FD   |              分区信息   4(16字节)              |
		   |------------------------------------------------|
		   | 01FE                    | 01FF                 |
		   |          55             |            AA        |
		   |------------------------------------------------|
			Figure 1: Boot Sector(主引导分区)的具体结图
 */
