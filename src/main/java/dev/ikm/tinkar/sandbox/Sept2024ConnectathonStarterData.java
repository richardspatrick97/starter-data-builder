package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.composer.Attachable;
import dev.ikm.tinkar.composer.Composer;
import dev.ikm.tinkar.composer.Session;
import dev.ikm.tinkar.composer.assembler.ConceptAssembler;
import dev.ikm.tinkar.composer.assembler.PatternAssembler;
import dev.ikm.tinkar.composer.assembler.SemanticAssembler;
import dev.ikm.tinkar.composer.template.FullyQualifiedName;
import dev.ikm.tinkar.composer.template.Identifier;
import dev.ikm.tinkar.composer.template.StatedAxiom;
import dev.ikm.tinkar.composer.template.Synonym;
import dev.ikm.tinkar.composer.template.USDialect;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.State;
import dev.ikm.tinkar.terms.TinkarTerm;
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

    private final File exportFile;
    private final UUIDUtility uuidUtility = new UUIDUtility();
    private final Session session;

    public Sept2024ConnectathonStarterData(File exportDataStore, File exportFile) {
        this.exportFile = exportFile;

        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, exportDataStore);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();

        Composer composer = new Composer("Sept 2024 Connectathon Starter Data");
        session = composer.open(State.ACTIVE, TinkarTerm.USER, TinkarTerm.PRIMORDIAL_MODULE, TinkarTerm.PRIMORDIAL_PATH);

        configureConceptsAndPatterns();

        composer.commitSession(session);

        exportStarterData();
        PrimitiveData.stop();
    }

    private void configureConceptsAndPatterns() {
        createConcept(Concept.make("Performance", UUID.fromString("395cc864-7c51-4072-b3e7-f9195b40053a")))
                .attach((StatedAxiom axiom) -> axiom.isA(TinkarTerm.MODEL_CONCEPT));

        configureConnectathonPatterns();
        configureValueContraintSemantics();
    }

    private void configureConnectathonPatterns() {
        createPresenceOfCovidPattern();
        createAbsenceOfCovidPattern();
        createUndeterminedCovidTestResultPattern();
        createInvalidCovidTestResultPattern();
    }

    private void createInvalidCovidTestResultPattern() {
        Concept invalid = createConceptFromSnomed("Invalid", "455371000124106");

        session.compose((PatternAssembler patternAssembler) -> patternAssembler
                .pattern(EntityProxy.Pattern.make("Invalid Covid Test Result Pattern", uuidUtility.createUUID("Invalid Covid Test Result Pattern")))
                .meaning(invalid)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .attach((Synonym synonym) -> synonym
                        .text("Invalid Covid Test Result Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach((USDialect usDialect) -> usDialect.acceptability(TinkarTerm.PREFERRED)))
        );

    }

    private void createUndeterminedCovidTestResultPattern() {
        String undeterminedResultStr = "Undetermined Covid Test Result Pattern";

        Concept undetermined = createConceptFromSnomed("Undetermined", "373068000");
        session.compose((PatternAssembler patternAssembler) -> patternAssembler
                .pattern(EntityProxy.Pattern.make(undeterminedResultStr, uuidUtility.createUUID(undeterminedResultStr)))
                .meaning(undetermined)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .attach((Synonym synonym) -> synonym
                        .text(undeterminedResultStr)
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach((USDialect usDialect) -> usDialect.acceptability(TinkarTerm.PREFERRED)))
        );
    }

    private void createAbsenceOfCovidPattern() {
        Concept absenceFindings = createConceptFromSnomed("Absence findings", "272519000");
        Concept absent = createConceptFromSnomed("Absent", "2667000");
        Concept negative = createConceptFromSnomed("Negative", "260385009");
        Concept notDetected = createConceptFromSnomed("Not Detected", "260415000");

        String patternStr = "Absence Of Covid Pattern";
        EntityProxy.Pattern pattern = EntityProxy.Pattern.make(patternStr, uuidUtility.createUUID(patternStr));
        session.compose((PatternAssembler patternAssembler) -> patternAssembler
                .pattern(pattern)
                .meaning(absenceFindings)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .attach((Synonym synonym) -> synonym
                        .text(patternStr)
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach((USDialect usDialect) -> usDialect.acceptability(TinkarTerm.PREFERRED)))
        );

        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(pattern)
                .reference(absent)
                .fieldValues(MutableList::newEmpty));

        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(pattern)
                .reference(negative)
                .fieldValues(MutableList::newEmpty));

        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(pattern)
                .reference(notDetected)
                .fieldValues(MutableList::newEmpty));
    }

    private void createPresenceOfCovidPattern() {
        Concept presenceFindings = createConceptFromSnomed("Presence findings", "260411009");
        Concept present = createConceptFromSnomed("Present", "52101004");
        Concept detected = createConceptFromSnomed("Detected", "260373001");
        Concept positive = createConceptFromSnomed("Positive", "10828004");
        Concept presumptivePositive = createConceptFromSnomed("Presumptive Positive", "720735008");

        String patternStr = "Presence of Covid Pattern";
        EntityProxy.Pattern pattern = EntityProxy.Pattern.make(patternStr, uuidUtility.createUUID(patternStr));
        session.compose((PatternAssembler patternAssembler) -> patternAssembler
                .pattern(pattern)
                .meaning(presenceFindings)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .attach((Synonym synonym) -> synonym
                        .text(patternStr)
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach((USDialect usDialect) -> usDialect.acceptability(TinkarTerm.PREFERRED)))
        );

        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(pattern)
                .reference(positive)
                .fieldValues(MutableList::newEmpty));

        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(pattern)
                .reference(present)
                .fieldValues(MutableList::newEmpty));

        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(pattern)
                .reference(detected)
                .fieldValues(MutableList::newEmpty));

        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(pattern)
                .reference(presumptivePositive)
                .fieldValues(MutableList::newEmpty));
    }

    private void configureValueContraintSemantics() {
        Concept cdcField1 = Concept.make("CDC", UUID.nameUUIDFromBytes("LP207920-2".getBytes()));
        createConcept(cdcField1, "Center For Disease Control and Prevention");

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

        Concept concept = createConceptFromSnomed(description, snomedctId);

        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(TinkarTerm.VALUE_CONSTRAINT_PATTERN)
                .reference(concept)
                .fieldValues((MutableList<Object> values) -> values
                        .with(cdcField1)
                        .with(TinkarTerm.GREATER_THAN_OR_EQUAL_TO)
                        .with(referenceRangeMinimum)
                        .with(maxRefRangeOperator)
                        .with(referenceRangeMaximum)
                        .with("lb/in^2"))
        );
    }

    private EntityProxy.Concept createConceptFromSnomed(String description, String snomedctId) {
        Concept concept = EntityProxy.Concept.make(description, UuidUtil.fromSNOMED(snomedctId));
        createConcept(concept);

        return concept;
    }

    private Attachable createConcept(Concept concept, String fullyQualifiedName) {
        return session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler
                .concept(concept)
                .attach((FullyQualifiedName fqn) -> fqn
                        .language(ENGLISH_LANGUAGE)
                        .text(fullyQualifiedName)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach((USDialect usDialect) -> usDialect.acceptability(TinkarTerm.PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text(concept.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach((USDialect usDialect) -> usDialect.acceptability(TinkarTerm.PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(concept.asUuidArray()[0].toString())));
    }

    private Attachable createConcept(Concept concept) {
        return createConcept(concept, concept.description());
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
