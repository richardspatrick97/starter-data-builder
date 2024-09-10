package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.id.PublicId;
import dev.ikm.tinkar.common.id.PublicIds;
import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.entity.*;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.TinkarTerm;
import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.MutableList;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;

public class Sept2024ConnectathonStarterData {
    private static File exportFile;

    public static void main(String[] args){

        TinkarStarterData.main(args);
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
        Concept snomedAuthor = Concept.make("IHTSDO SNOMED CT Author", uuidUtility.createUUID("IHTSDO SNOMED CT Author"));
        starterData.concept(snomedAuthor)
                .fullyQualifiedName("IHTSDO SNOMED CT Author", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT Author", TinkarTerm.PREFERRED)
                .definition("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT Author", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        Concept snomedIdentifier = Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedIdentifier)
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        configureConnectathonPatterns( starterData,  uuidUtility);

    }

    protected static void configureValueContraintSemantics(StarterData starterData, UUIDUtility uuidUtility){
        Concept snomedAuthor = Concept.make("IHTSDO SNOMED CT Author", uuidUtility.createUUID("IHTSDO SNOMED CT Author"));
        starterData.concept(snomedAuthor)
                .fullyQualifiedName("IHTSDO SNOMED CT Author", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT Author", TinkarTerm.PREFERRED)
                .definition("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT Author", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        Concept snomedIdentifier = Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedIdentifier)
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

    }

    protected static void configureConnectathonPatterns(StarterData starterData, UUIDUtility uuidUtility){

        createPresenceOfCovidPattern(starterData, uuidUtility);

        createAbsenceOfCovidPattern(starterData, uuidUtility);

        createUndeterminedCovidTestResultPattern(starterData, uuidUtility);

        createInvalidCovidTestResultPattern(starterData, uuidUtility);


    }

    private static void createInvalidCovidTestResultPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept invalid = Concept.make("Undetermined", UuidUtil.fromSNOMED("455371000124106"));
        starterData.concept(invalid)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, invalid.asUuidArray()[0].toString())
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Invalid Covid Test Result Pattern", uuidUtility.createUUID("Invalid Covid Test Result Pattern")))
                .meaning(invalid)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();
    }

    private static void createUndeterminedCovidTestResultPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept undetermined = Concept.make("Undetermined", UuidUtil.fromSNOMED("373068000"));
        starterData.concept(undetermined)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, undetermined.asUuidArray()[0].toString())
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Undetermined Covid Test Result Pattern", uuidUtility.createUUID("Undetermined Covid Test Result Pattern")))
                .meaning(undetermined)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();
    }

    private static void createAbsenceOfCovidPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept absenceFindings = Concept.make("Absence findings", UuidUtil.fromSNOMED("272519000"));
        starterData.concept(absenceFindings)
                .synonym("Absence findings",TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, absenceFindings.asUuidArray()[0].toString())
                .build();

        Concept absent = Concept.make("Absent", UuidUtil.fromSNOMED("2667000"));
        starterData.concept(absent)
                .synonym("Absent",TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, absent.asUuidArray()[0].toString())
                .build();

        Concept negative = Concept.make("Negative", UuidUtil.fromSNOMED("260385009"));
        starterData.concept(negative)
                .synonym("Negative",TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, negative.asUuidArray()[0].toString())
                .build();

        Concept notDetected = Concept.make("Not Detected", UuidUtil.fromSNOMED("260415000"));
        starterData.concept(notDetected)
                .synonym("Not Detected", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, notDetected.asUuidArray()[0].toString())
                .build();



        String pattern = "Absence Of Covid Pattern";
        starterData.pattern( EntityProxy.Pattern.make(pattern , uuidUtility.createUUID(pattern)))
                .meaning(absenceFindings)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        addConceptToSemanticPatterns(absent,pattern,starterData);
        addConceptToSemanticPatterns(negative,pattern,starterData);
        addConceptToSemanticPatterns(notDetected,pattern,starterData);
    }

    private static void createPresenceOfCovidPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept presenceFindings = Concept.make("Presence findings", UuidUtil.fromSNOMED("260411009"));
        starterData.concept(presenceFindings)
                .synonym("Presence Findings", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, presenceFindings.asUuidArray()[0].toString())
                .build();

        Concept present = Concept.make("Present", UuidUtil.fromSNOMED("52101004"));
        starterData.concept(present)
                .synonym("Present",TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, present.asUuidArray()[0].toString())
                .build();

        Concept detected = Concept.make("Detected", UuidUtil.fromSNOMED("260373001"));
        starterData.concept(detected)
                .synonym("Detected", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, detected.asUuidArray()[0].toString())
                .build();

        Concept positive = Concept.make("Positive", UuidUtil.fromSNOMED("10828004"));
        starterData.concept(positive)
                .synonym("Positive",TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, positive.asUuidArray()[0].toString())
                .build();

        Concept presumptivePositive = Concept.make("Presumptive Positive", UuidUtil.fromSNOMED("720735008"));
        starterData.concept(presumptivePositive)
                .synonym("Presumptive Positive",TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, presumptivePositive.asUuidArray()[0].toString())
                .build();


        String pattern = "Presence of Covid Pattern";

        starterData.pattern( EntityProxy.Pattern.make(pattern, uuidUtility.createUUID(pattern)))
                .meaning(presenceFindings)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        addConceptToSemanticPatterns(positive,pattern,starterData);
        addConceptToSemanticPatterns(present,pattern,starterData);
        addConceptToSemanticPatterns(detected,pattern,starterData);
        addConceptToSemanticPatterns(presumptivePositive,pattern,starterData);
    }

    private static void exportStarterData(){
        ExportEntitiesController exportEntitiesController = new ExportEntitiesController();
        try {
            exportEntitiesController.export(exportFile).get();
        } catch (ExecutionException | InterruptedException e){
            e.printStackTrace();
        }
    }
    private static void addConceptToSemanticPatterns(EntityProxy.Concept concept, String semanticPattern, StarterData starterData) {

        MutableList<Object> classPatternFields = Lists.mutable.empty();

        UUIDUtility uuidUtility = new UUIDUtility();
        PublicId patternPublicId = PublicIds.of(uuidUtility.createUUID(semanticPattern));
        int patternNid = EntityService.get().nidForPublicId(patternPublicId);
        PublicId referencedComponentPublicID = concept.publicId();
        int referencedComponentNid = EntityService.get().nidForPublicId(referencedComponentPublicID);
        PublicId semantic = PublicIds.singleSemanticId(patternPublicId, referencedComponentPublicID);
        int semanticNid = EntityService.get().nidForPublicId(semantic);
        UUID primordialUUID = semantic.asUuidArray()[0];
        int stampNid = EntityService.get().nidForPublicId(starterData.getAuthoringSTAMP());

        writeSemantic(semanticNid, primordialUUID, patternNid, referencedComponentNid, stampNid, classPatternFields);

    }
    private static void writeSemantic(int semanticNid, UUID primordialUUID, int patternNid, int referencedComponentNid, int stampNid, MutableList<Object> lidrRecordFields) {
        /************
         * Below: Creates the semantic with one version and write it to the database
         */
        //Create empty version list
        RecordListBuilder<SemanticVersionRecord> versions = RecordListBuilder.make();

        //Create Semantic Chronology
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(semanticNid)
                .leastSignificantBits(primordialUUID.getLeastSignificantBits())
                .mostSignificantBits(primordialUUID.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(patternNid)
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        //Create Semantic Version
        SemanticVersionRecord semanticVersionRecord = SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(stampNid)
                .fieldValues(lidrRecordFields.toImmutable())
                .build();

        versions.add(semanticVersionRecord);

        //Rebuild the Semantic with the now populated version data
        SemanticEntity<? extends SemanticEntityVersion> semanticEntity = SemanticRecordBuilder
                .builder(semanticRecord)
                .versions(versions.toImmutable()).build();
        EntityService.get().putEntity(semanticEntity);

    }
}
