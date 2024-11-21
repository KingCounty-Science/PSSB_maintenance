# PSSB_maintenance
Created by Beth A. Sosik, spring/summer 2024.

In July 2024, BAS created scripts to be used as tools for future PSSB admins to help keep STE and taxa mapping assignments up to date in the PSSB database. 

The mapping_update_screening_tool.R script is a utility to help PSSB admins manage the taxa translator table in PSSB. It compares the whole PSSB dataset against the latest version of the TaxaTranslator table from the BCG working group (Contact is Sean Sullivan of Rhithron), and against the mapping table built into PSSB. It hones in on four things:

1. It identifies taxa in PSSB samples that have a translation available from the BCG workgroup, but that translation is not within the PSSB mapping table and must be added. 

2. It identifies taxa in PSSB samples that do not have a BCG translation, but do have a mapping assigned in PSSB. These are generally because of slight naming convention differences between PSSB names and BCG names. These generally do have a mapping in BCG, but the PSSB mapping table follows the intention of the mapping rather than the literal exact name. These names can be provided to Sean Sullivan for addition or modification in the next BCG translator table.

3. It identifies taxa in PSSB samples that do not have a BCG translation OR a PSSB mapping assigned. Send these to Sean Sullivan once a year for updating the BCG translation tables, and then update the PSSB mapping tables once an update is available.

4. It checks for differences in the recommended mapping between the BCG translator table and the PSSB mapping table. Inspect these and see if the PSSB table needs to be edited to reflect current mapping recommendations.

The STE_update_screening_tool.R script is a now obsolete tool. It used the latest combined STE lookup table downloaded from PSSB, the latest BCG translation table and the latest BCG attribute table to see which taxa need to be added to the STE lookup tables in PSSB. This was developed in anticipation of the PSSB update, when it was assumed that there needed to be a 1:1 match between taxa in samples and taxa in the STE lookup tables. However, it was subsequently revealed that the STE lookup process in PSSB can search recursively up taxonomic hierarchies, and broad rules can be applied to the whole dataset with very few entries in the lookup table. The result is that STE tables do not need periodic updating to reflect new taxa additions, and once set will only need modification for a future re-calibration process.

I did not create a screening tool to check the PSSB attribute table for updates. This is because any substantial updates to the attributes should occur only as part of a comprehensive B-IBI recalibration effort. Instead, admins should use the mapping_update_screening_tool to note if new or changed OTU names should be added to the attributes table, with attributes assigned from old taxa names. Work with Sean Sullivan from Rhithron to confirm attributes can be copied from old taxon name to new OTU name without issue. 
