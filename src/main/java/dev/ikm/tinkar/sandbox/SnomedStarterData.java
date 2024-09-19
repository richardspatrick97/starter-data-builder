package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.List;
import java.util.concurrent.ExecutionException;

public class SnomedStarterData {
    private static File exportFile;

    public static void main(String[] args){
        File exportDataStore = new File(args[0]);
        exportFile = new File(args[1]);
        UUIDUtility uuidUtility = new UUIDUtility();

        //Build, export, and shutdown database
        StarterData starterData = new StarterData(exportDataStore, uuidUtility)
                .init()
                .authoringSTAMP(
                        TinkarTerm.ACTIVE_STATE,
                        System.currentTimeMillis(),
                        TinkarTerm.USER,
                        TinkarTerm.PRIMORDIAL_MODULE,
                        TinkarTerm.PRIMORDIAL_PATH);

        configureConceptsAndPatterns(starterData, uuidUtility);
        starterData.build(); //Natively writing data to spined array
        exportStarterData();  //exports starter data to pb.zip
        starterData.shutdown();
    }

    protected static void configureConceptsAndPatterns(StarterData starterData, UUIDUtility uuidUtility){
        Concept snomedAuthor = EntityProxy.Concept.make("IHTSDO SNOMED CT Author", uuidUtility.createUUID("IHTSDO SNOMED CT Author"));
        starterData.concept(snomedAuthor)
                .fullyQualifiedName("IHTSDO SNOMED CT Author", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT Author", TinkarTerm.PREFERRED)
                .definition("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT Author", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        Concept snomedIdentifier = EntityProxy.Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedIdentifier)
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

//        Concept snomedGrouper = EntityProxy.Concept.make("SNOMED CT Concept", TinkarTerm.HEALTH_CONCEPT.asUuidArray()[1]);//"SNOMED CT Concept", uuidUtility.createUUID("SNOMED CT Concept"));
//        starterData.concept(snomedGrouper)
////                .fullyQualifiedName("SNOMED CT Concept", TinkarTerm.PREFERRED)
////                .synonym("Health Concept", TinkarTerm.PREFERRED)
////                .definition("A grouper concept that contains the SNOMED CT hierarchy", TinkarTerm.PREFERRED)
////                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedGrouper.asUuidArray()[0].toString())
//                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
//                .build();
    }

    private static void exportStarterData(){
        ExportEntitiesController exportEntitiesController = new ExportEntitiesController();
        try {
            exportEntitiesController.export(exportFile).get();
        } catch (ExecutionException | InterruptedException e){
            e.printStackTrace();
        }
    }
}
