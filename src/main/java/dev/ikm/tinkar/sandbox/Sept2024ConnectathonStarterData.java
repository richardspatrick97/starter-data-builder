package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.id.PublicId;
import dev.ikm.tinkar.common.id.PublicIds;
import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.composer.Composer;
import dev.ikm.tinkar.composer.Session;
import dev.ikm.tinkar.composer.assembler.ConceptAssembler;
import dev.ikm.tinkar.composer.template.FullyQualifiedName;
import dev.ikm.tinkar.composer.template.Identifier;
import dev.ikm.tinkar.composer.template.StatedAxiom;
import dev.ikm.tinkar.composer.template.Synonym;
import dev.ikm.tinkar.composer.template.USDialect;
import dev.ikm.tinkar.entity.EntityService;
import dev.ikm.tinkar.entity.RecordListBuilder;
import dev.ikm.tinkar.entity.SemanticEntity;
import dev.ikm.tinkar.entity.SemanticEntityVersion;
import dev.ikm.tinkar.entity.SemanticRecord;
import dev.ikm.tinkar.entity.SemanticRecordBuilder;
import dev.ikm.tinkar.entity.SemanticVersionRecord;
import dev.ikm.tinkar.entity.SemanticVersionRecordBuilder;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.State;
import dev.ikm.tinkar.terms.TinkarTerm;
import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.MutableList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.UUID;
import java.util.concurrent.ExecutionException;

import static dev.ikm.tinkar.terms.TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE;
import static dev.ikm.tinkar.terms.TinkarTerm.ENGLISH_LANGUAGE;

public class Sept2024ConnectathonStarterData {

    private static final Logger log = LoggerFactory.getLogger(Sept2024ConnectathonStarterData.class);
    private static final String EXAMPLE_UUCM = "lb/in^2";

    private final File exportFile;
    private final StarterData starterData;
    private final UUIDUtility uuidUtility = new UUIDUtility();

    private final Composer composer = new Composer("Sept 2024 Connectathon Starter Data");

    public Sept2024ConnectathonStarterData(File exportDataStore, File exportFile) {
        this.exportFile = exportFile;

        starterData = new StarterData(exportDataStore, uuidUtility)
                .init()
                .authoringSTAMP(
                        TinkarTerm.ACTIVE_STATE,
                        System.currentTimeMillis(),
                        TinkarTerm.USER,
                        TinkarTerm.PRIMORDIAL_MODULE,
                        TinkarTerm.PRIMORDIAL_PATH);

        createComposerConcepts();
        configureConceptsAndPatterns();
        starterData.build();
        exportStarterData();

        starterData.shutdown();
    }

    private void createComposerConcepts() {
        Session session = composer.open(State.ACTIVE,
                System.currentTimeMillis(),
                TinkarTerm.USER,
                TinkarTerm.PRIMORDIAL_MODULE,
                TinkarTerm.PRIMORDIAL_PATH);

        Concept performance = Concept.make("Performance", UUID.fromString("395cc864-7c51-4072-b3e7-f9195b40053a"));
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler
                .concept(performance)
                .attach((FullyQualifiedName fqn) -> fqn
                        .language(ENGLISH_LANGUAGE)
                        .text("Performance")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach((USDialect usDialect) -> usDialect.acceptability(TinkarTerm.PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Performance")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach((USDialect usDialect) -> usDialect.acceptability(TinkarTerm.PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(performance.asUuidArray()[0].toString()))
                .attach((StatedAxiom axiom) -> axiom
                        .isA(TinkarTerm.MODEL_CONCEPT))
        );
        composer.commitSession(session);
    }

    private void configureConceptsAndPatterns() {
        configureConnectathonPatterns();
        configureValueContraintSemantics();
    }

    private void configureValueContraintSemantics() {
        Concept cdcField1 = Concept.make("CDC", UUID.nameUUIDFromBytes("LP207920-2".getBytes()));
        starterData.concept(cdcField1)
                .synonym("CDC", TinkarTerm.PREFERRED)
                .fullyQualifiedName("Center For Disease Control and Prevention", TinkarTerm.US_ENGLISH_DIALECT)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, cdcField1.asUuidArray()[0].toString())
                .build();

        createBMISemantic("Underweight (finding)", "248342006", cdcField1, 0F, TinkarTerm.LESS_THAN, 18.5F);
        createBMISemantic("Normal weight (finding)", "43664005", cdcField1, 18.5F, TinkarTerm.LESS_THAN_OR_EQUAL_TO, 24.99F);
        createBMISemantic("Obese (finding)", "414915002", cdcField1, 30F, TinkarTerm.LESS_THAN, 500F);
        createBMISemantic("Body mass index 30+ - obesity (finding)", "162864005", cdcField1, 30F, TinkarTerm.LESS_THAN, 500F);
        createBMISemantic("Obese class I (finding) (Body mass index 30.00 to 34.99)", "443371000124107", cdcField1, 30F, TinkarTerm.LESS_THAN_OR_EQUAL_TO, 34.99F);
        createBMISemantic("Obese class II (finding) ( Body mass index 35.00 to 39.99)", "443381000124105", cdcField1, 35F, TinkarTerm.LESS_THAN_OR_EQUAL_TO, 39.99F);
        createBMISemantic("Body mass index 40+ - severely obese (finding)", "408512008", cdcField1, 40F, TinkarTerm.LESS_THAN, 500F);
        createBMISemantic("Obese class III (finding) (Body mass index equal to or greater than 40)", "819948005", cdcField1, 40F, TinkarTerm.LESS_THAN, 500F);
    }

    private void createBMISemantic(String description, String snomedctId, Concept cdcField1, float referenceRangeMinimum,
                                   Concept maxRefRangeOperator, float referenceRangeMaximum) {

        Concept concept = makeBmiConcept(description, snomedctId);

        MutableList<Object> classPatternFields = Lists.mutable.empty();
        classPatternFields.add(cdcField1);
        classPatternFields.add(TinkarTerm.GREATER_THAN_OR_EQUAL_TO);
        classPatternFields.add(referenceRangeMinimum);
        classPatternFields.add(maxRefRangeOperator);
        classPatternFields.add(referenceRangeMaximum);
        classPatternFields.add(EXAMPLE_UUCM);

        int patternNid = EntityService.get().nidForPublicId(TinkarTerm.VALUE_CONSTRAINT_PATTERN);
        PublicId referencedComponentPublicID = concept.publicId();
        int referencedComponentNid = EntityService.get().nidForPublicId(referencedComponentPublicID);
        PublicId semantic = PublicIds.singleSemanticId(TinkarTerm.VALUE_CONSTRAINT_PATTERN, referencedComponentPublicID);
        int semanticNid = EntityService.get().nidForPublicId(semantic);
        UUID primordialUUID = semantic.asUuidArray()[0];
        int stampNid = EntityService.get().nidForPublicId(starterData.getAuthoringSTAMP());

        writeSemantic(semanticNid, primordialUUID, patternNid, referencedComponentNid, stampNid, classPatternFields);
    }

    private void configureConnectathonPatterns() {
        createPresenceOfCovidPattern();
        createAbsenceOfCovidPattern();
        createUndeterminedCovidTestResultPattern();
        createInvalidCovidTestResultPattern();
    }

    private void createInvalidCovidTestResultPattern() {
        Concept invalid = Concept.make("Invalid", UuidUtil.fromSNOMED("455371000124106"));
        starterData.concept(invalid)
                .synonym("Invalid", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, invalid.asUuidArray()[0].toString())
                .build();

        starterData.pattern(EntityProxy.Pattern.make("Invalid Covid Test Result Pattern", uuidUtility.createUUID("Invalid Covid Test Result Pattern")))
                .meaning(invalid)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();
    }

    private void createUndeterminedCovidTestResultPattern() {
        Concept undetermined = Concept.make("Undetermined", UuidUtil.fromSNOMED("373068000"));
        starterData.concept(undetermined)
                .synonym("Undetermined", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, undetermined.asUuidArray()[0].toString())
                .build();

        starterData.pattern(EntityProxy.Pattern.make("Undetermined Covid Test Result Pattern", uuidUtility.createUUID("Undetermined Covid Test Result Pattern")))
                .meaning(undetermined)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();
    }

    private void createAbsenceOfCovidPattern() {
        Concept absenceFindings = Concept.make("Absence findings", UuidUtil.fromSNOMED("272519000"));
        starterData.concept(absenceFindings)
                .synonym("Absence findings", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, absenceFindings.asUuidArray()[0].toString())
                .build();

        Concept absent = Concept.make("Absent", UuidUtil.fromSNOMED("2667000"));
        starterData.concept(absent)
                .synonym("Absent", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, absent.asUuidArray()[0].toString())
                .build();

        Concept negative = Concept.make("Negative", UuidUtil.fromSNOMED("260385009"));
        starterData.concept(negative)
                .synonym("Negative", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, negative.asUuidArray()[0].toString())
                .build();

        Concept notDetected = Concept.make("Not Detected", UuidUtil.fromSNOMED("260415000"));
        starterData.concept(notDetected)
                .synonym("Not Detected", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, notDetected.asUuidArray()[0].toString())
                .build();

        String pattern = "Absence Of Covid Pattern";
        starterData.pattern(EntityProxy.Pattern.make(pattern, uuidUtility.createUUID(pattern)))
                .meaning(absenceFindings)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        addConceptToSemanticPatterns(absent, pattern);
        addConceptToSemanticPatterns(negative, pattern);
        addConceptToSemanticPatterns(notDetected, pattern);
    }

    private void createPresenceOfCovidPattern() {
        Concept presenceFindings = Concept.make("Presence findings", UuidUtil.fromSNOMED("260411009"));
        starterData.concept(presenceFindings)
                .synonym("Presence Findings", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, presenceFindings.asUuidArray()[0].toString())
                .build();

        Concept present = Concept.make("Present", UuidUtil.fromSNOMED("52101004"));
        starterData.concept(present)
                .synonym("Present", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, present.asUuidArray()[0].toString())
                .build();

        Concept detected = Concept.make("Detected", UuidUtil.fromSNOMED("260373001"));
        starterData.concept(detected)
                .synonym("Detected", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, detected.asUuidArray()[0].toString())
                .build();

        Concept positive = Concept.make("Positive", UuidUtil.fromSNOMED("10828004"));
        starterData.concept(positive)
                .synonym("Positive", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, positive.asUuidArray()[0].toString())
                .build();

        Concept presumptivePositive = Concept.make("Presumptive Positive", UuidUtil.fromSNOMED("720735008"));
        starterData.concept(presumptivePositive)
                .synonym("Presumptive Positive", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, presumptivePositive.asUuidArray()[0].toString())
                .build();


        String pattern = "Presence of Covid Pattern";

        starterData.pattern(EntityProxy.Pattern.make(pattern, uuidUtility.createUUID(pattern)))
                .meaning(presenceFindings)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        addConceptToSemanticPatterns(positive, pattern);
        addConceptToSemanticPatterns(present, pattern);
        addConceptToSemanticPatterns(detected, pattern);
        addConceptToSemanticPatterns(presumptivePositive, pattern);
    }

    private void addConceptToSemanticPatterns(EntityProxy.Concept concept, String semanticPattern) {

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

    private void writeSemantic(int semanticNid, UUID primordialUUID, int patternNid, int referencedComponentNid, int stampNid, MutableList<Object> lidrRecordFields) {
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

    private EntityProxy.Concept makeBmiConcept(String description, String snomedctId) {
        Concept concept = EntityProxy.Concept.make(description, UuidUtil.fromSNOMED(snomedctId));
        starterData.concept(concept)
                .synonym(description, TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, concept.asUuidArray()[0].toString())
                .build();

        return concept;
    }

    private void exportStarterData() {
        ExportEntitiesController exportEntitiesController = new ExportEntitiesController();
        try {
            exportEntitiesController.export(exportFile).get();
        } catch (ExecutionException | InterruptedException e) {
            log.error("Unexpected exception while exporting", e);
        }
    }

    public static void main(String[] args) {
        new Sept2024ConnectathonStarterData(new File(args[0]), new File(args[1]));
    }

}
