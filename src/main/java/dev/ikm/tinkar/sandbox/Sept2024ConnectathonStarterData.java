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
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;

public class Sept2024ConnectathonStarterData {
    public static final String EXAMPLE_UUCM = "lb/in^2";
    //public static final String EXAMPLE_UUCM = "kg/m^2";
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

        Concept performance = Concept.make("Performance", UUID.fromString("395cc864-7c51-4072-b3e7-f9195b40053a"));
        starterData.concept(performance)
                .fullyQualifiedName("Performance", TinkarTerm.PREFERRED)
                .synonym("Performance", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, performance.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        configureConnectathonPatterns( starterData,  uuidUtility);

        configureValueContraintSemantics(starterData,uuidUtility);
    }

    protected static void configureValueContraintSemantics(StarterData starterData, UUIDUtility uuidUtility){

        Concept cdcField1 = Concept.make("CDC", UUID.nameUUIDFromBytes("LP207920-2".getBytes()));
        starterData.concept(cdcField1)
                .synonym("CDC",TinkarTerm.PREFERRED)
                .fullyQualifiedName("Center For Disease Control and Prevention",TinkarTerm.US_ENGLISH_DIALECT)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, cdcField1.asUuidArray()[0].toString())
                .build();

        BMIConcept concept = new BMIConcept("248342006", "Underweight (finding)");
        Concept bmiConcept = getBmiConcept(starterData, concept);
        createBMISematic(starterData, bmiConcept, cdcField1, TinkarTerm.GREATER_THAN_OR_EQUAL_TO,  0F, TinkarTerm.LESS_THAN, 18.5F, EXAMPLE_UUCM);

        concept = new BMIConcept("43664005", "Normal weight (finding)");
        bmiConcept = getBmiConcept(starterData, concept);
        createBMISematic(starterData, bmiConcept, cdcField1, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, 18.5F, TinkarTerm.LESS_THAN_OR_EQUAL_TO, 24.99F, EXAMPLE_UUCM);

        concept = new BMIConcept("414915002", "Obese (finding)");
        bmiConcept = getBmiConcept(starterData, concept);
        createBMISematic(starterData, bmiConcept, cdcField1, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, 30F, TinkarTerm.LESS_THAN, 500F, EXAMPLE_UUCM);

        concept = new BMIConcept("162864005", "Body mass index 30+ - obesity (finding)");
        bmiConcept = getBmiConcept(starterData, concept);
        createBMISematic(starterData, bmiConcept, cdcField1, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, 30F, TinkarTerm.LESS_THAN, 500F, EXAMPLE_UUCM);

        concept = new BMIConcept("443371000124107", "Obese class I (finding) (Body mass index 30.00 to 34.99)");
        bmiConcept = getBmiConcept(starterData, concept);
        createBMISematic(starterData, bmiConcept, cdcField1, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, 30F, TinkarTerm.LESS_THAN_OR_EQUAL_TO,  34.99F, EXAMPLE_UUCM);

        concept = new BMIConcept("443381000124105", "Obese class II (finding) ( Body mass index 35.00 to 39.99)");
        bmiConcept = getBmiConcept(starterData, concept);
        createBMISematic(starterData, bmiConcept, cdcField1, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, 35F, TinkarTerm.LESS_THAN_OR_EQUAL_TO, 39.99F, EXAMPLE_UUCM);

        concept = new BMIConcept("408512008", "Body mass index 40+ - severely obese (finding)");
        bmiConcept = getBmiConcept(starterData, concept);
        createBMISematic(starterData, bmiConcept, cdcField1, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, 40F, TinkarTerm.LESS_THAN, 500F, EXAMPLE_UUCM);

        concept = new BMIConcept("819948005", "Obese class III (finding) (Body mass index equal to or greater than 40)");
        bmiConcept = getBmiConcept(starterData, concept);
        createBMISematic(starterData, bmiConcept, cdcField1, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, 40F, TinkarTerm.LESS_THAN, 500F, EXAMPLE_UUCM);
    }

    private static Concept getBmiConcept(StarterData starterData, BMIConcept concept) {
        Concept bmiConcept = concept.makeConcept();
        starterData.concept(bmiConcept)
                .synonym(concept.getDecription(),TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, bmiConcept.asUuidArray()[0].toString())
                .build();
        return bmiConcept;
    }

    private static void createBMISematic(StarterData starterData,
                                         Concept bmiConcept, Concept cdcField1,
                                         Concept minRefRangeOperator, float referenceRangeMinimum,
                                         Concept maxRefRangeOperator, float referenceRangeMaximum, String exampleUUCM) {

        MutableList<Object> classPatternFields = Lists.mutable.empty();
        classPatternFields.add(cdcField1);
        classPatternFields.add(minRefRangeOperator);
        classPatternFields.add(referenceRangeMinimum);
        classPatternFields.add(maxRefRangeOperator);
        classPatternFields.add(referenceRangeMaximum);
        classPatternFields.add(exampleUUCM);

        //UUIDUtility uuidUtility = new UUIDUtility();
        //PublicId patternPublicId = PublicIds.of(uuidUtility.createUUID("Value Constraint Pattern"));
        int patternNid = EntityService.get().nidForPublicId(TinkarTerm.VALUE_CONSTRAINT_PATTERN);
        PublicId referencedComponentPublicID = bmiConcept.publicId();
        int referencedComponentNid = EntityService.get().nidForPublicId(referencedComponentPublicID);
        PublicId semantic = PublicIds.singleSemanticId(TinkarTerm.VALUE_CONSTRAINT_PATTERN, referencedComponentPublicID);
        int semanticNid = EntityService.get().nidForPublicId(semantic);
        UUID primordialUUID = semantic.asUuidArray()[0];
        int stampNid = EntityService.get().nidForPublicId(starterData.getAuthoringSTAMP());

        writeSemantic(semanticNid, primordialUUID, patternNid, referencedComponentNid, stampNid, classPatternFields);
    }

    protected static void configureConnectathonPatterns(StarterData starterData, UUIDUtility uuidUtility){

        createPresenceOfCovidPattern(starterData, uuidUtility);

        createAbsenceOfCovidPattern(starterData, uuidUtility);

        createUndeterminedCovidTestResultPattern(starterData, uuidUtility);

        createInvalidCovidTestResultPattern(starterData, uuidUtility);
    }

    private static void createInvalidCovidTestResultPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept invalid = Concept.make("Invalid", UuidUtil.fromSNOMED("455371000124106"));
        starterData.concept(invalid)
                .synonym("Invalid",TinkarTerm.PREFERRED)
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
                .synonym("Undetermined",TinkarTerm.PREFERRED)
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
