package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.entity.Entity;
import dev.ikm.tinkar.entity.EntityVersion;
import dev.ikm.tinkar.entity.load.LoadEntitiesFromProtobufFile;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.StarterDataTerm;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

public class SnomedLoincStarterData {

    private static final Logger LOG = Logger.getLogger(SnomedLoincStarterData.class.getSimpleName());

    private static File exportDataStore;
    private static File importDataStore;
    private static File exportFile;

    private static Entity<? extends EntityVersion> authoringSTAMP;
    public static void main(String args[]){

        exportDataStore = new File(args[0]);
        exportFile = new File(args[1]);
        importDataStore = new File(args[2]);

        UUIDUtility uuidUtility = new UUIDUtility();

        StarterData starterData = new StarterData(exportDataStore, uuidUtility)
                .init()
                .authoringSTAMP(
                        TinkarTerm.ACTIVE_STATE,
                        System.currentTimeMillis(),
                        TinkarTerm.USER,
                        TinkarTerm.PRIMORDIAL_MODULE,
                        TinkarTerm.PRIMORDIAL_PATH);


        starterData.concept(TinkarTerm.COMPONENT_FIELD)
                .fullyQualifiedName("Component field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component field", TinkarTerm.PREFERRED)
                .definition("Component field", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        authoringSTAMP = starterData.getAuthoringSTAMP();

        configureConceptsAndPatterns(starterData, uuidUtility);
        starterData.build(); //Natively writing data to spined array
//        transformAnalysis(uuidUtility); //Isolate and inspect import and export transforms
//        exportStarterData();  //exports starter data to pb.zip
        starterData.shutdown();

        //Load exported starter data into clean database
        importStarterData(); //load pb.zip into database

    }

    private static void configureConceptsAndPatterns(StarterData starterData, UUIDUtility uuidUtility){


        EntityProxy.Concept snomedIdentifier = EntityProxy.Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedIdentifier)
                .fullyQualifiedName("SNOMED CT Identifier", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT ID", TinkarTerm.PREFERRED)
                .synonym("SCTID", TinkarTerm.PREFERRED)
                .definition("Unique point of origin for identifier", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        EntityProxy.Concept snomedGrouper = EntityProxy.Concept.make("SNOMED CT Concept", uuidUtility.createUUID("SNOMED CT Concept"));
        starterData.concept(snomedGrouper)
                .fullyQualifiedName("SNOMED CT Concept", TinkarTerm.PREFERRED)
                .synonym("Health Concept", TinkarTerm.PREFERRED)
                .definition("A grouper concept that contains the SNOMED CT hierarchy", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedGrouper.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        EntityProxy.Concept snomedAuthor = EntityProxy.Concept.make("SNOMED CT LOINC Collaboration Author", uuidUtility.createUUID("SNOMED CT LOINC Collaboration Author"));
        starterData.concept(snomedAuthor)
                .fullyQualifiedName("SNOMED CT LOINC Collaboration Author", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT LOINC Collaboration Author", TinkarTerm.PREFERRED)
                .definition("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT and LOINC Author", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        /* UUID from SNOMED Browser - This is the ECL query : '705114005 |LOINC Code System (qualifier value)|'  */
        EntityProxy.Concept loincIdentifier = EntityProxy.Concept.make("LOINC Number", UuidUtil.fromSNOMED("705114005"));
        starterData.concept(loincIdentifier)
                .fullyQualifiedName("LOINC Number", TinkarTerm.PREFERRED)
                .synonym("LOINC Num", TinkarTerm.PREFERRED)
                .definition("Unique point of origin for identifier", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        EntityProxy.Concept snomedctIdentifier = EntityProxy.Concept.make("SNOMED CT Identifier Source", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedctIdentifier)
                .fullyQualifiedName("SNOMED CT Identifier Source", TinkarTerm.PREFERRED)
                .synonym("ID starting point", TinkarTerm.PREFERRED)
                .definition("Unique point of origin for identifier", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();
    }

    /*
    Do these concepts from the excel file.

    SNOMED LOINC Collaboration Author
    LOINC Number
    SNOMED CT Identifier Source
     */

    private static void importStarterData() {
        LOG.info("Starting database");
        LOG.info("Loading data from " + importDataStore.getAbsolutePath());
        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, importDataStore);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();

        try {
            LoadEntitiesFromProtobufFile loadEntitiesFromProtobufFile = new LoadEntitiesFromProtobufFile(exportFile);
            loadEntitiesFromProtobufFile.call();
        }catch (Exception e){
            LOG.severe(e.getMessage());
            e.printStackTrace();
        }

        List<Integer> patternNids = new ArrayList<>();
        PrimitiveData.get().forEachPatternNid(patternNids::add);


        System.out.println("break");

        PrimitiveData.stop();
    }
}
