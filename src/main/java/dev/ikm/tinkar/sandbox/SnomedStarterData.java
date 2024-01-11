package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.entity.Entity;
import dev.ikm.tinkar.entity.EntityVersion;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.entity.load.LoadEntitiesFromProtobufFile;
import dev.ikm.tinkar.entity.transfom.EntityToTinkarSchemaTransformer;
import dev.ikm.tinkar.entity.transfom.TinkarSchemaToEntityTransformer;
import dev.ikm.tinkar.schema.TinkarMsg;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;


public class SnomedStarterData {

    private static final Logger LOG = Logger.getLogger(SnomedStarterData.class.getSimpleName());

    private static File exportDataStore;
    private static File importDataStore;
    private static File exportFile;

    private static Entity<? extends EntityVersion> authoringSTAMP;

    public static void main(String[] args){
        exportDataStore = new File(args[0]);
        exportFile = new File(args[1]);
        importDataStore = new File(args[2]);
//        FileUtil.recursiveDelete(exportDataStore);
//        FileUtil.recursiveDelete(importDataStore);
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
        Concept snomedAuthor = EntityProxy.Concept.make("IHTSDO SNOMED CT Author", uuidUtility.createUUID("IHTSDO SNOMED CT Author"));
        starterData.concept(snomedAuthor)
                .fullyQualifiedName("IHTSDO SNOMED CT Author", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT Author", TinkarTerm.PREFERRED)
                .definition("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT Author", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .inferredNavigation(null, List.of(TinkarTerm.USER))
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        Concept snomedIdentifier = EntityProxy.Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedIdentifier)
                .fullyQualifiedName("SNOMED CT Identifier", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT ID", TinkarTerm.PREFERRED)
                .synonym("SCTID", TinkarTerm.PREFERRED)
                .definition("Unique point of origin for identifier", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedIdentifier.asUuidArray()[0].toString())
                .inferredNavigation(null, List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .statedNavigation(null, List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        Concept snomedGrouper = EntityProxy.Concept.make("SNOMED CT Concept", uuidUtility.createUUID("SNOMED CT Concept"));
        starterData.concept(snomedGrouper)
                .fullyQualifiedName("SNOMED CT Concept", TinkarTerm.PREFERRED)
                .synonym("Health Concept", TinkarTerm.PREFERRED)
                .definition("A grouper concept that contains the SNOMED CT hierarchy", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedGrouper.asUuidArray()[0].toString())
                .inferredNavigation(List.of(TinkarTerm.PHENOMENON), List.of(TinkarTerm.ROOT_VERTEX))
                .statedNavigation(List.of(TinkarTerm.PHENOMENON), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

//        Optional<Entity<EntityVersion>> phenomenonVersion = EntityService.get().getEntity(TinkarTerm.PHENOMENON.asUuidArray()[0]);
//        if (phenomenonVersion.isEmpty()) {
//            throw new RuntimeException("OOPS...");
//        }
//        Entity<? extends EntityVersion> phenomenon = phenomenonVersion.get();
//        EntityService.get().getEntity(TinkarTerm.PHENOMENON.asUuidArray()[0]).get().versions()

//        RecordListBuilder<SemanticVersionRecord> versions = RecordListBuilder.make();
//        versions.add(SemanticVersionRecordBuilder.builder()
//                .chronology(semanticRecord)
//                .stampNid(starterData.getAuthoringSTAMP().nid())
//                .fieldValues(navigationFields.toImmutable())
//                .build());
//
//        SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
//
//
//
//        starterData.concept(TinkarTerm.PHENOMENON)
//                .definition("A unique thought, fact, or circumstance", TinkarTerm.PREFERRED)
//                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PHENOMENON.asUuidArray()[0].toString())
//                .inferredNavigation(List.of(TinkarTerm.IDENTIFIER_SOURCE), null)
//                .statedNavigation(List.of(TinkarTerm.IDENTIFIER_SOURCE), null)
//                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
//                .build();
    }

    private static void startDatabase(File dataStoreFile) {
        LOG.info("Starting database");
        LOG.info("Loading data from " + dataStoreFile.getAbsolutePath());
        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, dataStoreFile);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();
    }

    private static void stopDatabase() {
        PrimitiveData.stop();
    }

    private static void exportStarterData(){
        ExportEntitiesController exportEntitiesController = new ExportEntitiesController();
        try {
            exportEntitiesController.export(exportFile).get();
        } catch (ExecutionException | InterruptedException e){
            e.printStackTrace();
        }
    }

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

    private static void transformAnalysis(UUIDUtility uuidUtility){
        TinkarSchemaToEntityTransformer importTransform = TinkarSchemaToEntityTransformer.getInstance();
        EntityToTinkarSchemaTransformer exportTransform = EntityToTinkarSchemaTransformer.getInstance();
        int diTreeNid=  Entity.provider().semanticNidsOfPattern(TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN.nid())[0];
        Entity<? extends EntityVersion> originalDiTreeEntity = Entity.getFast(diTreeNid);

        //Export transform from Entity to TinkarMsg
        TinkarMsg tinkarSchemaDiTree = exportTransform.transform(originalDiTreeEntity);

        //Import transform from TinkarMsg to Entity
        final List<Entity<? extends EntityVersion>> entities = new ArrayList<>();
        importTransform.transform(tinkarSchemaDiTree, entities::add, stampEntity -> {});

    }
}
