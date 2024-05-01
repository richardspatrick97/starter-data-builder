package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;

public class SnomedLoincStarterData {

    private static final Logger LOG = Logger.getLogger(SnomedLoincStarterData.class.getSimpleName());

    private static File exportDataStore;
    private static File exportFile;
    public static void main(String args[]){

        exportDataStore = new File(args[0]);
        exportFile = new File(args[1]);

        UUIDUtility uuidUtility = new UUIDUtility();

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
        exportStarterData(); //exports starter data to pb.zip
        starterData.shutdown();
    }

    protected static void configureConceptsAndPatterns(StarterData starterData, UUIDUtility uuidUtility){
        SnomedStarterData.configureConceptsAndPatterns(starterData, uuidUtility);

        /* UUID from SNOMED Browser - This is the ECL query : '705114005 |LOINC Code System (qualifier value)|'  */
        EntityProxy.Concept loincIdentifier = EntityProxy.Concept.make("LOINC Number", UuidUtil.fromSNOMED("705114005"));
        starterData.concept(loincIdentifier)
                .fullyQualifiedName("LOINC Number", TinkarTerm.PREFERRED)
                .synonym("LOINC Num", TinkarTerm.PREFERRED)
                .definition("Unique point of origin for identifier", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();
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
