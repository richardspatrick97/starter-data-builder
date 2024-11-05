package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.io.FileUtil;
import dev.ikm.tinkar.common.util.time.DateTimeUtil;
import dev.ikm.tinkar.composer.Composer;
import dev.ikm.tinkar.composer.Session;
import dev.ikm.tinkar.composer.assembler.ConceptAssembler;
import dev.ikm.tinkar.composer.assembler.PatternAssembler;
import dev.ikm.tinkar.composer.assembler.SemanticAssembler;
import dev.ikm.tinkar.composer.template.Definition;
import dev.ikm.tinkar.composer.template.FullyQualifiedName;
import dev.ikm.tinkar.composer.template.Identifier;
import dev.ikm.tinkar.composer.template.KometBaseModel;
import dev.ikm.tinkar.composer.template.StatedAxiom;
import dev.ikm.tinkar.composer.template.StatedNavigation;
import dev.ikm.tinkar.composer.template.Synonym;
import dev.ikm.tinkar.composer.template.TinkarBaseModel;
import dev.ikm.tinkar.composer.template.USDialect;
import dev.ikm.tinkar.entity.EntityService;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.State;
import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.MutableList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.concurrent.ExecutionException;

import static dev.ikm.tinkar.terms.TinkarTerm.*;


public class TinkarStarterData {

    private static final Logger LOG = LoggerFactory.getLogger(TinkarStarterData.class.getSimpleName());

    private final File exportFile;
    private final File datastore;

    public TinkarStarterData(String[] args) {
        datastore = new File(args[0]);
        exportFile = new File(args[1]);
        FileUtil.recursiveDelete(datastore);
    }

    private void init() {
        LOG.info("Starting database");
        LOG.info("Loading data from " + datastore.getAbsolutePath());
        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, datastore);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();
    }

    private void cleanup() {
        PrimitiveData.stop();
    }

    private void transform() {
        EntityService.get().beginLoadPhase();
        try {
            Composer composer = new Composer("Tinkar Starter Data Composer");
            Session session = composer.open(
                    State.ACTIVE,
                    PrimitiveData.PREMUNDANE_TIME,
                    USER,
                    PRIMORDIAL_MODULE,
                    PRIMORDIAL_PATH);
            createConcepts(session);
            createPatterns(session);

            createPathMembershipSemantics(session);
            addPathOriginSemantics(session);

            composer.commitSession(session);
        } finally {
            EntityService.get().endLoadPhase();
        }
    }

    private void createConcepts(Session session) {
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ENGLISH_DIALECT_ASSEMBLAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("English Dialect")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("English dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Specifies the dialect of the English language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ENGLISH_DIALECT_ASSEMBLAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(GB_ENGLISH_DIALECT, US_ENGLISH_DIALECT)
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Text comparison measure semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Text comparison")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Text comparison with a focus on semantic meaning involves evaluating the similarity or relatedness between pieces of text based on their underlying meaning rather than just their surface structure.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TEXT_COMPARISON_MEASURE_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(CASE_INSENSITIVE_EVALUATION, CASE_SENSITIVE_EVALUATION)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STARTER_DATA_AUTHORING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(STARTER_DATA_AUTHORING.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Metadata Authoring")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Define necessary minimum viable concepts to use Tinkar Data")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STARTER_DATA_AUTHORING.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(AXIOM_SYNTAX))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(AXIOM_SYNTAX.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Axiom Syntax")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Syntax defining description logic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(AXIOM_SYNTAX.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EXPRESS_AXIOM_SYNTAX))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(EXPRESS_AXIOM_SYNTAX.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Express Axiom")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Expressing description logic through syntax")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EXPRESS_AXIOM_SYNTAX.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(AXIOM_SYNTAX))
                .attach(new StatedAxiom()
                        .isA(AXIOM_SYNTAX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ACCEPTABLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Acceptable (foundation metadata concept)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Acceptable")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Specifies that a description is acceptable, but not preferred within a language or dialect.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ACCEPTABLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_ACCEPTABILITY))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_ACCEPTABILITY)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ACTIVE_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Active state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Active")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are active.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ACTIVE_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ALLOWED_STATES_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Allowed states for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Allowed states")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Predefined list of values for STAMP coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ALLOWED_STATES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(AND))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("And (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("And")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("An operator that typically is employed to combine two conditions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(AND.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ANNOTATION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Annotation type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Annotation type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Metadata about program elements, and annotation types define the structure of these annotations")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ANNOTATION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(COMMENT, KOMET_ISSUE)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ANONYMOUS_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Anonymous concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Anonymous concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concepts or entities that do not have a specific, named identity, (defined on-the-fly without a dedicated name)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ANONYMOUS_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCEPT_TYPE))
                .attach(new StatedAxiom()
                        .isA(CONCEPT_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ANY_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Any component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Any component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A general-purpose container to represent any component with generic data structure. Modifiable based on the specific requirements and characteristics of the components.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ANY_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ARRAY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Array (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Array")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Linear data structure")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ARRAY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ARRAY_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Array field (Solor)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Array field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A lexical set of semantically related elements/items")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ARRAY_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(AUTHOR_FOR_EDIT_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Individual or entity who made a particular edit or revision in a document (authoring a specific location or point in the codebase where an edit was made)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(AUTHOR_FOR_EDIT_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(AUTHOR_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Individual or entity who made a specific set of changes or modifications to a codebase/terminology resulting in the creation of a new version or revision")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(AUTHOR_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(AUTHORS_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Authors")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("In individual or an entity responsible for defining or updating the values associated with the STAMP coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(AUTHORS_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(AXIOM_FOCUS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Axiom focus (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Axiom focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A statement or proposition that is assumed to be true without requiring proof, it serves as a foundation principles on which a system or theory is built. Focus refers to the central point of attention or concentration on a specific concept/axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(AXIOM_FOCUS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(COMPONENT_TYPE_FOCUS))
                .attach(new StatedAxiom()
                        .isA(COMPONENT_TYPE_FOCUS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(AXIOM_ORIGIN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Axiom origin (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Axiom origin")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The parent concept for the axiom?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(AXIOM_ORIGIN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(STATED_PREMISE_TYPE, INFERRED_PREMISE_TYPE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(BOOLEAN_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("True (1) or false (0)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(BOOLEAN_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(BOOLEAN_LITERAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean literal (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean literal")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("TRUE, FALSE, UNKNOWN")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(BOOLEAN_LITERAL.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LITERAL_VALUE))
                .attach(new StatedAxiom()
                        .isA(LITERAL_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(BOOLEAN_REFERENCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean reference (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean reference")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Reference(a pointer) to a Boolean object")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(BOOLEAN_REFERENCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(QUERY_CLAUSES))
                .attach(new StatedAxiom()
                        .isA(QUERY_CLAUSES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(BOOLEAN_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The process of replacing or substituting boolean values or expression in a logical context")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(BOOLEAN_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(FIELD_SUBSTITUTION))
                .attach(new StatedAxiom()
                        .isA(FIELD_SUBSTITUTION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(BYTE_ARRAY_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Byte array field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Byte array field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("An array of bytes")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(BYTE_ARRAY_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CANCELED_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Canceled state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Canceled")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are canceled")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CANCELED_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CASE_INSENSITIVE_EVALUATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Case insensitive evaluation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Ignore case")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Evaluates values regardless of the case")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CASE_INSENSITIVE_EVALUATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .attach(new StatedAxiom()
                        .isA(TEXT_COMPARISON_MEASURE_SEMANTIC)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CASE_SENSITIVE_EVALUATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Case sensitive evaluation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Compare case")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Evaluated based on the case")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CASE_SENSITIVE_EVALUATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .attach(new StatedAxiom()
                        .isA(TEXT_COMPARISON_MEASURE_SEMANTIC)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Case significance concept nid for description (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Case significance")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A field label which captures the case significance for a given concept description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CHINESE_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Chinese language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Chinese language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Chinese language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CHINESE_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CHRONICLE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Chronicle properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Chronicle properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Attributes or characteristic associated with a historical record or an account of events (metadata, timestamps)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CHRONICLE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(PRIMORDIAL_UUID_FOR_CHRONICLE, VERSION_LIST_FOR_CHRONICLE, SEMANTIC_LIST_FOR_CHRONICLE, UUID_LIST_FOR_COMPONENT)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(COMMENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Comment (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Comment")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A filed label to capture free text information which may be necessary to add or change (concepts, relationships, semantics, etc)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(COMMENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ANNOTATION_TYPE))
                .attach(new StatedAxiom()
                        .isA(ANNOTATION_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(COMPONENT_ID_LIST_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component Id list")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Component Id list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A display field that references an ordered list of Concept IDs.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(COMPONENT_ID_LIST_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(COMPONENT_ID_SET_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component Id set field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Component Id set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A display field that references an unordered list of Concept IDs.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(COMPONENT_ID_SET_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(COMPONENT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Component field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A display field type that references a concept ID.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(COMPONENT_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(COMPONENT_FOR_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component for semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Component for semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(COMPONENT_FOR_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(COMPONENT_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Component Semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Component semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(COMPONENT_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_TYPE))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(COMPONENT_TYPE_FOCUS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component type focus (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Component type focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Focus type of component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(COMPONENT_TYPE_FOCUS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(AXIOM_FOCUS, CONCEPT_FOCUS, DESCRIPTION_FOCUS)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_CONSTRAINTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept constraints(SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept constraints")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Defined filters for a given concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_CONSTRAINTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ACTION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(ACTION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_DETAILS_TREE_TABLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept details tree table (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept details tree table")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Tree table with concept details")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_DETAILS_TREE_TABLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Field for the human readable description for the given concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_FOCUS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept focus (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_FOCUS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(COMPONENT_TYPE_FOCUS))
                .attach(new StatedAxiom()
                        .isA(COMPONENT_TYPE_FOCUS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_REFERENCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept reference (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept reference")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A field to capture a reference to validate concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_REFERENCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Value to define a given semantic as a concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_TYPE))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Substitution for concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(FIELD_SUBSTITUTION))
                .attach(new StatedAxiom()
                        .isA(FIELD_SUBSTITUTION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_TO_FIND))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept to find (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept to find")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Find concept (if searching on Komet shows us the results 'details and further information?)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_TO_FIND.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ACTION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(ACTION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A field that captures a defined concept label")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ANONYMOUS_CONCEPT, PATH_CONCEPT, SEMANTIC_FIELD_CONCEPTS)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Version")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A filed that captures the version of the terminology that it came from")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCRETE_DOMAIN_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concrete value operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concrete value operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A concept that specifies value operators")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCRETE_DOMAIN_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(EQUAL_TO, GREATER_THAN, GREATER_THAN_OR_EQUAL_TO, LESS_THAN, LESS_THAN_OR_EQUAL_TO, MAXIMUM_VALUE_OPERATOR, MINIMUM_VALUE_OPERATOR)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONDITIONAL_TRIGGERS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Conditional triggers (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Conditional triggers")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Conditional triggers based on actions, reasoner")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONDITIONAL_TRIGGERS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ACTION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(ACTION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONNECTIVE_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Connective operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Connective operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A field that captures what the operator is (logical connective)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONNECTIVE_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(AND, DEFINITION_ROOT, DISJOINT_WITH, OR, IS_A, PART_OF, CONCEPT_REFERENCE)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CORELATION_EXPRESSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Correlation expression (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Correlation expression")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A value for Correlation properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CORELATION_EXPRESSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CORRELATION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CORRELATION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CORELATION_REFERENCE_EXPRESSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Correlation reference expression (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Correlation reference expression")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A value for correlation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CORELATION_REFERENCE_EXPRESSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CORRELATION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CORRELATION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CORRELATION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Correlation properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Correlation properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Characteristics or measures that describe the relationship between two or more variables")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CORRELATION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(CORELATION_EXPRESSION, CORELATION_REFERENCE_EXPRESSION)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CREATIVE_COMMONS_BY_LICENSE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Creative Commons BY license (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Creative Commons BY license")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Creative Commons (CC) licenses are a set of public copyright licenses that enable the free distribution of an otherwise copyrighted work")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CREATIVE_COMMONS_BY_LICENSE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CZECH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Czech dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Czech dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Czech dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CZECH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CZECH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Czech language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Czech language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Czech Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CZECH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DANISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Danish language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Danish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Danish Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DANISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DEFAULT_MODULE_FOR_EDIT_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Default module for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Default module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A value for coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DEFAULT_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DEFINITION_DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Definition description type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Semantic value describing the description type for the description pattern is a definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DEFINITION_DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DEFINITION_ROOT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Definition root (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Definition root")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DEFINITION_ROOT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Human readable text for a concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_ACCEPTABILITY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description acceptability")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description acceptability")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Whether a given human readable text for a concept is permissible")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_ACCEPTABILITY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ACCEPTABLE, PREFERRED)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_CASE_SENSITIVE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description case sensitive")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Case sensitive")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Assumes the description is dependent on capitalization")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_CASE_SENSITIVE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_CASE_SIGNIFICANCE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description case significance")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description case significance")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Specifies how to handle the description text in terms of case sensitivity")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_CASE_SIGNIFICANCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(DESCRIPTION_CASE_SENSITIVE, DESCRIPTION_NOT_CASE_SENSITIVE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_CORE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description core type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description core type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Used to mark non-snomed descriptions as one of the core snomed types")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_CORE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_DIALECT_PAIR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description dialect pair (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description dialect pair")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Description dialect pair - linking together dialects with language descriptions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_DIALECT_PAIR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR, DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR)
                        .parents(DESCRIPTION_VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_FOCUS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description focus (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Description focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_FOCUS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(COMPONENT_TYPE_FOCUS))
                .attach(new StatedAxiom()
                        .isA(COMPONENT_TYPE_FOCUS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description for dialect/description pair (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description for dialect/description pair")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Linking together dialects with language descriptions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_DIALECT_PAIR))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_DIALECT_PAIR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description initial character case sensitive (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Initial character case insensitive")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Value which designates initial character as sensitive for a given description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_CASE_SIGNIFICANCE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description logic profile for logic coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Logic profile")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description not case sensitive")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Case insensitive")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Value which designate character as not sensitive for a given description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_NOT_CASE_SENSITIVE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_CASE_SIGNIFICANCE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description semantic")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Purpose and meaning for the description pattern and dialect patterns")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Specifying what type of description it is i.e. is it fully qualified or regular and etc.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(DEFINITION_DESCRIPTION_TYPE, FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE, REGULAR_NAME_DESCRIPTION_TYPE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_TYPE_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description type for description (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Linking for each description -> what type it is")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_TYPE_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description type preference list for language coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Type order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_VERSION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description version properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description version properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Combination of terms that might be used in a specific context or domain")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_VERSION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION, DESCRIPTION_TYPE_FOR_DESCRIPTION, LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION, DESCRIPTION_DIALECT_PAIR)
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_LOGIC_PROFILE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description-logic profile (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description-logic profile")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_LOGIC_PROFILE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(EL_PLUS_PLUS_PROFILE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESTINATION_MODULE_FOR_EDIT_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Destination module for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Destination module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESTINATION_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DEVELOPMENT_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Development module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Development module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Predefines or standard module within a system or application that is specifically designed to support the development phase of a project")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DEVELOPMENT_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MODULE))
                .attach(new StatedAxiom()
                        .isA(MODULE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DEVELOPMENT_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Development path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Development path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A path that specifies that the components are currently under development")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DEVELOPMENT_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH))
                .attach(new StatedAxiom()
                        .isA(PATH)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIGRAPH_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("DiGraph field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Instant/ DiGraph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A display field that references a di-graph whose edges are ordered pairs of vertices. Each edge can be followed from one vertex to another vertex.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIGRAPH_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DITREE_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("DiTree field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("DiTree")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A display field that references a graph obtained from an undirected tree by replacing each undirected edge by two directed edges with opposite directions.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DITREE_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dialect for dialect/description pair (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Dialect for dialect/description pair")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Specific dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_DIALECT_PAIR))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_DIALECT_PAIR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIGRAPH_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Digraph for logic coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Digraph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A value which describes a immutable coordinate property")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIGRAPH_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIRECTED_GRAPH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Directed graph (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("NavigationCoordinate/Directed graph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIRECTED_GRAPH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(EL_PLUS_PLUS_DIGRAPH)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DISJOINT_WITH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Disjoint with (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Disjoint with")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("An operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DISJOINT_WITH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DISPLAY_FIELDS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Display Fields")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Display fields")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Captures the human readable terms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DISPLAY_FIELDS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(COMPONENT_FIELD, COMPONENT_ID_LIST_FIELD, COMPONENT_ID_SET_FIELD, CONCEPT_FIELD, DIGRAPH_FIELD, DITREE_FIELD, FLOAT_FIELD, INTEGER_FIELD, SEMANTIC_FIELD_TYPE, STRING)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DOUBLE_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Double field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Double field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A data value (type of structure for data)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DOUBLE_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DUTCH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dutch language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Dutch language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Dutch language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DUTCH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EL_PROFILE_SET_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("El profile set operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("EL profile set operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("EL profile set operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EL_PROFILE_SET_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(NECESSARY_SET, SUFFICIENT_SET)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ terminological axioms")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ terminological axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The set of relationships or axioms has defined by the EL++ Logic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("El++ Inferred Concept Definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ Inferred Concept Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LOGICAL_DEFINITION))
                .attach(new StatedAxiom()
                        .isA(LOGICAL_DEFINITION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Inferred terminological axioms")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ Inferred terminological axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(NECESSARY_SET, SUFFICIENT_SET, INCLUSION_SET, ROLE, ROLE_GROUP)
                        .parents(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Stated Concept Definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ Stated Concept Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LOGICAL_DEFINITION))
                .attach(new StatedAxiom()
                        .isA(LOGICAL_DEFINITION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Stated terminological axioms")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ Stated terminological axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(NECESSARY_SET, SUFFICIENT_SET, INCLUSION_SET, ROLE, ROLE_GROUP)
                        .parents(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EL_PLUS_PLUS_DIGRAPH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ digraph (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ digraph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The directed graph that results from classifying a set of EL++ axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EL_PLUS_PLUS_DIGRAPH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIRECTED_GRAPH))
                .attach(new StatedAxiom()
                        .isA(DIRECTED_GRAPH)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EL_PLUS_PLUS_PROFILE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ logic profile (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("EL ++ logic profile")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("EL ++ profile")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EL_PLUS_PLUS_PROFILE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_LOGIC_PROFILE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_LOGIC_PROFILE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ENGLISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("English Language")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("English language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Value for description language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ENGLISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EQUAL_TO))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Equal to (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Equal to")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \"=\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EQUAL_TO.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EXACT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Exact (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Exact")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Source and target are semantic or exact lexical match")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EXACT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(GROUPING))
                .attach(new StatedAxiom()
                        .isA(GROUPING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EXTENDED_DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Extended description type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Extended description type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Used to store non-snomed description types when other terminologies are imported")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EXTENDED_DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EXTENDED_RELATIONSHIP_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Extended relationship type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Extended relationship type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Used to store non-snomed relationship types when other terminologies are imported- especially when a relationship is mapped onto a snomed relationship type (such as isa)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EXTENDED_RELATIONSHIP_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FLOAT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Float field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Represents values as high-precision fractional values.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FLOAT_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FLOAT_LITERAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float literal (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Float literal")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Numbers with decimal point or an exponential part")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FLOAT_LITERAL.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LITERAL_VALUE))
                .attach(new StatedAxiom()
                        .isA(LITERAL_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FLOAT_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Float substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FLOAT_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(FIELD_SUBSTITUTION))
                .attach(new StatedAxiom()
                        .isA(FIELD_SUBSTITUTION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FRENCH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("French dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("French dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("French dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FRENCH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FRENCH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("French Language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("French language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("French Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FRENCH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Fully qualified name description type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Fully qualified name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Fully qualified name is a description that uniquely identifies and differentiates it from other concepts with similar descriptions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(GB_ENGLISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Great Britain English dialect")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("GB English dialect / GB English")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Great Britain: English Language reference set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(GB_ENGLISH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ENGLISH_DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(ENGLISH_DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(GERMAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("German Language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("German language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("German Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(GERMAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(GREATER_THAN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Greater than (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Greater than")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \">\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(GREATER_THAN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(GREATER_THAN_OR_EQUAL_TO))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Greater than or equal to (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Greater than or equal to")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \">=\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(GREATER_THAN_OR_EQUAL_TO.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(GROUPING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Health concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Health concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(GROUPING.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(EXACT, PARTIAL)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(HEALTH_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Health concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Health concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(HEALTH_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IDENTIFIER_SOURCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Identifier Source")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Identifier source")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("An identifier used to label the identity of a unique component.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IDENTIFIER_SOURCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INCLUSION_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inclusion set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Inclusion set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A set of relationships that indicate something is has an inclusion. Not necessarily or sufficient but inclusive.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INCLUSION_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INACTIVE_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inactive state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Inactive")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are no longer active")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INACTIVE_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INFERRED_PREMISE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inferred premise type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred relationship / Inferred")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The axiom view following the application of the reasoner")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INFERRED_PREMISE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(AXIOM_ORIGIN))
                .attach(new StatedAxiom()
                        .isA(AXIOM_ORIGIN)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INSTANT_LITERAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Instant literal (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Instant literal")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("May refer to a specific point in time which is often represented by a date or time value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INSTANT_LITERAL.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LITERAL_VALUE))
                .attach(new StatedAxiom()
                        .isA(LITERAL_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INSTANT_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Instant substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Instant substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Substitution of instant literal?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INSTANT_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(FIELD_SUBSTITUTION))
                .attach(new StatedAxiom()
                        .isA(FIELD_SUBSTITUTION)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INTEGER_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Integer Field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Integer field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data type that represents some range of mathematical integers")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INTEGER_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INVERSE_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inverse name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Inverse name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("This is the extended description type that maye be attached to a description within a concept that defines as Association refex to signify that the referenced description  is the inverse of the association name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INVERSE_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INVERSE_TREE_LIST))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inverse tree list (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Inverse tree list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Inverse tree list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INVERSE_TREE_LIST.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TREE_AMALGAM_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TREE_AMALGAM_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IRISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Irish dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Irish dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Irish dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IRISH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IRISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Irish language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Irish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Irish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IRISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IS_A))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Is-a")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Is a")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Designates the parent child relationship")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IS_A.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IS_A_INFERRED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Is-a inferred navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Is-a inferred navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Designates the parent child relationship following the application of the reasoner")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IS_A_INFERRED_NAVIGATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(NAVIGATION))
                .attach(new StatedAxiom()
                        .isA(NAVIGATION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IS_A_STATED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Is-a stated navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Is-a stated navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Designates the parent child relationship as authored")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IS_A_STATED_NAVIGATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(NAVIGATION))
                .attach(new StatedAxiom()
                        .isA(NAVIGATION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ITALIAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Italian Language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Italian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Italian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ITALIAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(KOMET_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("KOMET module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("KOMET module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Komet specific values?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(KOMET_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MODULE))
                .attach(new StatedAxiom()
                        .isA(MODULE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(KOMET_USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("KOMET user (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("KOMET user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Authorized to author, edit and/or view in Komet")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(KOMET_USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(KOMET_USER_LIST))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("KOMET user list (SOLOR")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("KOMET user list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Inventory of authorized komet users")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(KOMET_USER_LIST.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(KOMET_ISSUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Komet issue (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Komet issue")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Komet being the 'annotation type' - specified type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(KOMET_ISSUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ANNOTATION_TYPE))
                .attach(new StatedAxiom()
                        .isA(ANNOTATION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(KOREAN_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Korean dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Korean dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Korean dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(KOREAN_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(STANDARD_KOREAN_DIALECT)
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(KOREAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Korean Language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Korean language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Korean language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(KOREAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Specifies the language of the description text.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ENGLISH_LANGUAGE, SPANISH_LANGUAGE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language concept nid for description (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Language for description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Captures the language code for a description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LANGUAGE_COORDINATE_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language coordinate name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Language coordinate name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LANGUAGE_COORDINATE_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LANGUAGE_COORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language coordinate properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Language coordinate properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Spatial representation of language, attributes or language coordinates, programming language metadata?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LANGUAGE_COORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(LANGUAGE_COORDINATE_NAME, DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LANGUAGE_NID_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language nid for language coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Language nid")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language specification for language coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LESS_THAN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Less than (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Less than")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \"<\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LESS_THAN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LESS_THAN_OR_EQUAL_TO))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Less than or equal to (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Less than or equal to")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \"<=\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LESS_THAN_OR_EQUAL_TO.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LITHUANIAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Lithuanian language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Lithuanian Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Lithuanian Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LITHUANIAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGIC_COORDINATE_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logic coordinate name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Logic coordinate name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGIC_COORDINATE_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LOGIC_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(LOGIC_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGIC_COORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logic coordinate properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Logic coordinate properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Structural characteristics of logical elements, Attributes of Logical coordinates, Mathematical Representation of logical relationships ?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGIC_COORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(LOGIC_COORDINATE_NAME)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGICAL_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logical Definition")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Logical Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The semantic value describing the purpose of the stated and inferred terminological axioms.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGICAL_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGICAL_EXPRESSION_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logical expression field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Logical expression field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGICAL_EXPRESSION_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGICAL_EXPRESSION_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logical expression semantic  (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Logical expression semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGICAL_EXPRESSION_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_TYPE))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGICALLY_EQUIVALENT_TO))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logically equivalent to (Solor)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Logically equivalent to")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("An operator for the reasoner to determine the equivalence")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGICALLY_EQUIVALENT_TO.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TAXONOMY_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(TAXONOMY_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MASTER_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Master path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Master path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A default path for components")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MASTER_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH))
                .attach(new StatedAxiom()
                        .isA(PATH)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MEANING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Meaning")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Meaning")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The interpretation or explanation field for a pattern/semantics")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MEANING.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Membership semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Membership semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Membership semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MEMBERSHIP_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_TYPE))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODEL_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Model concept")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Model concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(" ")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODEL_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TINKAR_MODEL_CONCEPT)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TINKAR_MODEL_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Tinkar Model concept")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Tinkar Model concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(" ")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TINKAR_MODEL_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(AXIOM_ORIGIN, CONCRETE_DOMAIN_OPERATOR, DESCRIPTION, DESCRIPTION_ACCEPTABILITY,
                                DESCRIPTION_CASE_SIGNIFICANCE, DESCRIPTION_SEMANTIC, DESCRIPTION_TYPE,
                                DIALECT_ASSEMBLAGE, DISPLAY_FIELDS, EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS,
                                IDENTIFIER_SOURCE, IDENTIFIER_VALUE, INFERRED_DEFINITION, IS_A,
                                LANGUAGE, LOGICAL_DEFINITION, MEANING, PURPOSE,
                                PHENOMENON, RELATIONSHIP_DESTINATION, RELATIONSHIP_ORIGIN,
                                REFERENCE_RANGE, STATED_DEFINITION, TEXT_FOR_DESCRIPTION, VALUE_CONSTRAINT,
                                VALUE_CONSTRAINT_SOURCE, AXIOM_SYNTAX)
                        .parents(MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(PRIMORDIAL_MODULE)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module exclusion set for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Module exclusions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Module exclusion set for stamp coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE_FOR_USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module for user (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Module for user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("User preference for Module?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE_FOR_USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Module Version")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE_OPTIONS_FOR_EDIT_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module options for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Module options")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Coordinate edit options for Module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE_OPTIONS_FOR_EDIT_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module preference list for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Module Preference list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Module preference list for stamp coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module preference list for language coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Module nids")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Module preference list for language coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module preference order for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Module order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Module preference order for stamp coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULES_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Modules for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Modules")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Stamp coordinate modules")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(NAVIGATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(NAVIGATION_CONCEPT_SET, NAVIGATION_VERTEX)
                        .parents(PURPOSE))
                .attach(new StatedAxiom()
                        .isA(PURPOSE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(NAVIGATION_CONCEPT_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Navigation concept set (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Navigation set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Navigating sets of concepts?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(NAVIGATION_CONCEPT_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(NAVIGATION))
                .attach(new StatedAxiom()
                        .isA(NAVIGATION)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(NAVIGATION_VERTEX))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Navigation vertex (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Navigation vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Navigation vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(NAVIGATION_VERTEX.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(NAVIGATION))
                .attach(new StatedAxiom()
                        .isA(NAVIGATION)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Necessary but not sufficient concept definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Necessary but not sufficient concept definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Not sufficiently defined by necessary conditions definition status")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(NECESSARY_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Necessary set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Necessary set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A set of relationships that is always true of a concept.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(NECESSARY_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(NOT_APPLICABLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Not Applicable (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Not applicable")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Not available")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(NOT_APPLICABLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_CASE_SIGNIFICANCE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(OBJECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Object (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Object")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("An encapsulation of data together with procedures")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(OBJECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(STATUS_VALUE, DESCRIPTION, NID, UNIVERSALLY_UNIQUE_IDENTIFIER, ANY_COMPONENT, UNINITIALIZED_COMPONENT, SANDBOX_COMPONENT, MODULE, PATH, OBJECT_PROPERTIES, HAS_ACTIVE_INGREDIENT, HAS_DOSE_FORM, LATERALITY)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(OBJECT_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Object Properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Object properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Objects are instances of classes, the properties describe the data or attributes that an object can have")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(OBJECT_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ACTION_PROPERTIES, CHRONICLE_PROPERTIES, VERSION_PROPERTIES, IMMUTABLECOORDINATE_PROPERTIES,
                                LANGUAGE_COORDINATE_PROPERTIES, LOGIC_COORDINATE_PROPERTIES, PATH_COORDINATE_PROPERTIES,
                                SEMANTIC_PROPERTIES, TREE_AMALGAM_PROPERTIES, CORRELATION_PROPERTIES, TRANSITIVE_PROPERTY,
                                REFLEXIVE_PROPERTY, ANNOTATION_PROPERTY_SET, DATA_PROPERTY_SET, PROPERTY_SEQUENCE_IMPLICATION)
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(OR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Or (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Or")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(OR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ORDER_FOR_AXIOM_ATTACHMENTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Order for axiom attachments (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Axiom attachment order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Order in which axioms are attached")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ORDER_FOR_AXIOM_ATTACHMENTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ORDER_FOR_CONCEPT_ATTACHMENTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Order for concept attachments  (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concept attachment order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Order in which concepts are attached")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ORDER_FOR_CONCEPT_ATTACHMENTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ORDER_FOR_DESCRIPTION_ATTACHMENTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Order for description attachments (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description attachment order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Order in which descriptions are attached")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ORDER_FOR_DESCRIPTION_ATTACHMENTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PART_OF))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Part of (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Part of")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Part of an attribute")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PART_OF.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PARTIAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Partial (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Partial")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Exists in/ Inclusion of ?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PARTIAL.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(GROUPING))
                .attach(new StatedAxiom()
                        .isA(GROUPING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A set of assets under version control that can be managed distinctly from other assets. Paths “branch” from other paths when established, and can be “merged” with other paths as well.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(DEVELOPMENT_PATH, MASTER_PATH, PRIMORDIAL_PATH, SANDBOX_PATH)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Path concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCEPT_TYPE))
                .attach(new StatedAxiom()
                        .isA(CONCEPT_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_COORDINATE_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path coordinate name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path coordinate name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Path coordinate name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_COORDINATE_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(PATH_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_COORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path coordinate properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path coordinate properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Character or attribute of coordinates referring to a series of connected points, that form a shape or trajectory")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_COORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(PATH_COORDINATE_NAME, PATH_ORIGINS)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_FOR_PATH_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path for path coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Path for path coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_FOR_PATH_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_FOR_USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path for user (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path for user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Path for user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_FOR_USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path for version")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Version path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_OPTIONS_FOR_EDIT_CORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path options for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path options")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Path options for edit coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_OPTIONS_FOR_EDIT_CORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_ORIGINS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path origins (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path origins")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Path origins")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_ORIGINS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(PATH_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH_ORIGINS_FOR_STAMP_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path origins for stamp path (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Path origins")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Path origins for stamp path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH_ORIGINS_FOR_STAMP_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PROMOTION_PATH_FOR_EDIT_CORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Promotion Path for Edit Coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Promotion Path for Edit Coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Promotion Path for Edit Coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PROMOTION_PATH_FOR_EDIT_CORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(PATH_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PHENOMENON))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Phenomenon")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Phenomenon")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A unique thought, fact, or circumstance")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PHENOMENON.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(EXAMPLE_UCUM_UNITS)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(POLISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Polish dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Polish dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Polish Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(POLISH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(POLISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Polish Language (Language)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Polish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Polish Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(POLISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PREFERRED))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Preferred (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Preferred")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Preferred( Foundation metadata concept)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PREFERRED.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_ACCEPTABILITY))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_ACCEPTABILITY)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PRESENTATION_UNIT_DIFFERENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Presentation unit different (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Presentation unit different")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Unit difference")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PRESENTATION_UNIT_DIFFERENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PRIMORDIAL_UUID_FOR_CHRONICLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Primordial UUID for chronicle (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Primordial UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Primordial UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PRIMORDIAL_UUID_FOR_CHRONICLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PRIMORDIAL_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Primordial module")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Primordial module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(" ")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PRIMORDIAL_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MODULE))
                .attach(new StatedAxiom()
                        .isA(MODULE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PRIMORDIAL_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Primordial path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Primordial path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(" ")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PRIMORDIAL_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH))
                .attach(new StatedAxiom()
                        .isA(PATH)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PRIMORDIAL_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Primordial state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Primordial")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that have not yet been released and exist in their most basic form.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PRIMORDIAL_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCED_COMPONENT_NID_FOR_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Referenced component nid for semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Referenced component id")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Component id Referenced")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCED_COMPONENT_NID_FOR_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCED_COMPONENT_SUBTYPE_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Referenced component subtype restriction (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Referenced component subtype restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Stores the (optional) referenced component type sub restriction selection which will be used by the validator to check the user input for the referenced component when creating an instance of a dynamic field.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(ROLE_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCED_COMPONENT_TYPE_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Referenced component type restriction (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Referenced component type restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Stores the (Optional) referenced component type restriction selection which will be used by the validator to check the user input for the referenced component when creating an instance of a dynamic field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCED_COMPONENT_TYPE_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(ROLE_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REGULAR_NAME_DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Regular name description type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Regular name description type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("There may be descriptions/synonyms marked as “regular.”")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REGULAR_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(RELATIONSHIP_DESTINATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Relationship destination")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Relationship destination")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Signifies path to child concepts which are more specific than the Tinkar term")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(RELATIONSHIP_DESTINATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(RELATIONSHIP_ORIGIN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Relationship origin")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Relationship origin")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Signifies path to parent concepts which are more general than the Tinkar term")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(RELATIONSHIP_ORIGIN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ROLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Role")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Is an abstract representation of a high-level role for a therapeutic medicinal product; the concepts are not intended to describe a detailed indication for therapeutic use nor imply that therapeutic use is appropriate in all clinical situations.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ROLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ROLE_TYPE, ROLE_OPERATOR, ROLE_RESTRICTION)
                        .parents(ROLE_GROUP, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(ROLE_GROUP, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ROLE_GROUP))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role group")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Role group")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("An association between a set of attribute or axiom value pairs that causes them to be considered together within a concept definition or post coordinated expression.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ROLE_GROUP.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ROLE)
                        .parents(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ROLE_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role operator")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Role operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concept that is used to describe universal vs existential restrictions.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ROLE_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(UNIVERSAL_RESTRICTION, EXISTENTIAL_RESTRICTION)
                        .parents(ROLE))
                .attach(new StatedAxiom()
                        .isA(ROLE));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ROLE_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(ROLE_RESTRICTION.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Role value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Role restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ROLE_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE))
                .attach(new StatedAxiom()
                        .isA(ROLE));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ROLE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Role type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Refers to a concept that represents a particular kind of relationship that can exist between two entities. It defines the specific function or responsibility that one entity plays in relation to another.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ROLE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE))
                .attach(new StatedAxiom()
                        .isA(ROLE));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ROLE_TYPE_TO_ADD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role type to add (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Role type to add")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Action - add role type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ROLE_TYPE_TO_ADD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ACTION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(ACTION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ROOT_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Root for logic coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Root")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Logic coordinate root")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ROOT_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(RUSSIAN_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Russian dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Russian dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Russian Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(RUSSIAN_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(RUSSIAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Russian language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Russian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Russian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(RUSSIAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SANDBOX_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Sandbox component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Sandbox component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SANDBOX_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(SANDBOX_MODULE, SANDBOX_PATH)
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SANDBOX_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Sandbox module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Sandbox module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SANDBOX_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(SANDBOX_PATH_MODULE)
                        .parents(MODULE, SANDBOX_COMPONENT))
                .attach(new StatedAxiom()
                        .isA(MODULE, SANDBOX_COMPONENT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SANDBOX_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Sandbox path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A path for components under testing.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SANDBOX_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH))
                .attach(new StatedAxiom()
                        .isA(PATH)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SANDBOX_PATH_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox path module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Sandbox Path module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Sandbox path module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SANDBOX_PATH_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SANDBOX_MODULE))
                .attach(new StatedAxiom()
                        .isA(SANDBOX_MODULE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SEMANTIC_FIELD_CONCEPTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field concepts (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic field concepts")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Semantic field concepts")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SEMANTIC_FIELD_CONCEPTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCEPT_TYPE))
                .attach(new StatedAxiom()
                        .isA(CONCEPT_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SEMANTIC_FIELD_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Field name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Field name - semantics")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SEMANTIC_FIELD_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SEMANTIC_FIELD_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic field type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("List of fields-  semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SEMANTIC_FIELD_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SEMANTIC_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The attributes or characteristics of a concept, term, or element that convey meaning or semantics in a given context")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SEMANTIC_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(REFERENCED_COMPONENT_NID_FOR_SEMANTIC, COMPONENT_FOR_SEMANTIC, LOGIC_GRAPH_FOR_SEMANTIC, SEMANTIC_FIELD_NAME)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SEMANTIC_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Type- semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SEMANTIC_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(COMPONENT_SEMANTIC, CONCEPT_SEMANTIC, DESCRIPTION_SEMANTIC, LOGICAL_EXPRESSION_SEMANTIC, MEMBERSHIP_SEMANTIC)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SIGNED_INTEGER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Signed integer (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Signed integer")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Signed integer (Foundation metadata concept)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SIGNED_INTEGER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SPANISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Spanish language")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Spanish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Value for the description language dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SPANISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STANDARD_KOREAN_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Standard Korean dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Standard Korean Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Standard")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STANDARD_KOREAN_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(KOREAN_DIALECT))
                .attach(new StatedAxiom()
                        .isA(KOREAN_DIALECT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATED_PREMISE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated premise type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Stated")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Stated relationship")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STATED_PREMISE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(AXIOM_ORIGIN))
                .attach(new StatedAxiom()
                        .isA(AXIOM_ORIGIN)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATUS_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Status for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Status for version")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Version status?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STATUS_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATUS_VALUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Status value")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Status")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The status of the STAMP Coordinate(Active, Cancelled, Inactive, Primordial)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STATUS_VALUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ACTIVE_STATE, CANCELED_STATE, INACTIVE_STATE, PRIMORDIAL_STATE, WITHDRAWN_STATE)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("String")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("String")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A sequence of characters, either as a literal constant or as a variable. Strings could be used to represent terms from code systems or URLs, textual definitions, etc.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STRING.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SUFFICIENT_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient concept definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Sufficient concept definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concept definition - Sufficient")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(SUFFICIENT_CONCEPT_DEFINITION_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient concept definition operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Sufficient concept definition operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concept definition operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(SUFFICIENT_CONCEPT_DEFINITION, NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SUFFICIENT_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Sufficient set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A set of relationships that differentiate a concept and its subtypes from all other concepts. A concept that contains at least one set of necessary and sufficient conditions is considered defined.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SUFFICIENT_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SWEDISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Swedish language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Swedish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Swedish Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SWEDISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TEXT_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Text for description")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Text")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Captures the human readable text for a description in Komet")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TEXT_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TIME_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Time for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Time for version")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Version time")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TIME_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TREE_AMALGAM_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Tree amalgam properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Tree amalgam properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data structure that consists of nodes connected by edges (a mixture or blend of different elements)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TREE_AMALGAM_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(INVERSE_TREE_LIST, TREE_LIST)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TREE_LIST))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Tree list (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Tree list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("List - Tree")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TREE_LIST.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TREE_AMALGAM_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TREE_AMALGAM_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(US_ENGLISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("United States of America English dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("US English dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("USA -english dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(US_ENGLISH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(US_NURSING_DIALECT)
                        .parents(ENGLISH_DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(ENGLISH_DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(US_NURSING_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("US Nursing dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("United States English Nursing Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Nursing Dialect -US English")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(US_NURSING_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(US_ENGLISH_DIALECT))
                .attach(new StatedAxiom()
                        .isA(US_ENGLISH_DIALECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UUID_DATA_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID data type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("UUID data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Distinction of data type of UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UUID_DATA_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UUID_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("UUID field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Universally unique identifier field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UUID_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UUID_LIST_FOR_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID list for component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("UUIDs")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("UUIDs")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UUID_LIST_FOR_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UNCATEGORIZED_PHENOMENON))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Uncategorized phenomenon (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Uncategorized phenomenon")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Unknown")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UNCATEGORIZED_PHENOMENON.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PHENOMENON))
                .attach(new StatedAxiom()
                        .isA(PHENOMENON)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UNINITIALIZED_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Uninitialized Component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Uninitialized")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Not initialized component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UNINITIALIZED_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UNIVERSAL_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Universal Restriction")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Universal Restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Universal restrictions constrain the relationships along a given property to concepts that are members of a specific class.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UNIVERSAL_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(ROLE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UNIVERSALLY_UNIQUE_IDENTIFIER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UNIVERSALLY_UNIQUE_IDENTIFIER")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A universally unique identifier that uniquely represents a concept in Tinkar")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UNIVERSALLY_UNIQUE_IDENTIFIER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IDENTIFIER_SOURCE))
                .attach(new StatedAxiom()
                        .isA(IDENTIFIER_SOURCE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(KOMET_USER, KOMET_USER_LIST, MODULE_FOR_USER, ORDER_FOR_AXIOM_ATTACHMENTS, ORDER_FOR_CONCEPT_ATTACHMENTS, ORDER_FOR_DESCRIPTION_ATTACHMENTS, PATH_FOR_USER, STARTER_DATA_AUTHORING)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VERSION_LIST_FOR_CHRONICLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Version list for chronicle (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Versions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Chronicle version list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VERSION_LIST_FOR_CHRONICLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VERSION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Version Properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Version properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VERSION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(AUTHOR_FOR_VERSION, MODULE_FOR_VERSION, PATH_FOR_VERSION, STATUS_FOR_VERSION, TIME_FOR_VERSION, DESCRIPTION_VERSION_PROPERTIES)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VERTEX_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Field for Vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VERTEX_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VERTEX_STATE_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex state set (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Vertex states")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Vertex states")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VERTEX_STATE_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VERTEX_SORT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex sort (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Sort")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Vertex sort")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VERTEX_SORT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VIEW_COORDINATE_KEY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("View coordinate key (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("View Key")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("View Key")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VIEW_COORDINATE_KEY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(QUERY_CLAUSES))
                .attach(new StatedAxiom()
                        .isA(QUERY_CLAUSES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(WITHDRAWN_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Withdrawn state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Withdrawn")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are withdrawn.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(WITHDRAWN_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(BOOLEAN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(BOOLEAN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(BYTE_ARRAY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Byte array (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Byte array")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(BYTE_ARRAY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_LIST_FOR_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description list for concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Description list for concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("List of description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_LIST_FOR_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DOUBLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Double (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Double")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DOUBLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FLOAT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Float")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FLOAT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGIC_GRAPH_FOR_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logic graph for semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Logic graph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGIC_GRAPH_FOR_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LONG))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Long (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Long")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LONG.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(NID))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("NID (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Native Identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(NID.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SEMANTIC_LIST_FOR_CHRONICLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic list for chronicle (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic list for chronicle")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Semantic list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SEMANTIC_LIST_FOR_CHRONICLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(USERS_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Users module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("User module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Module - user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(USERS_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MODULE))
                .attach(new StatedAxiom()
                        .isA(MODULE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ROOT_VERTEX))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Integrated Knowledge Management (SOLOR)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach(usDialect())
                .attach((new Synonym()
                        .text("Tinkar root concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)))
                .attach(usDialect())
                .attach((new Definition()
                        .text("Terminologies that are represented in a harmonized manner")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)))
                .attach(usDialect())
                .attach((new Identifier()
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER))
                        .identifier(ROOT_VERTEX.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(MODEL_CONCEPT, MEANING, OBJECT, ROLE, USER, ANNOTATION_TYPE, CREATIVE_COMMONS_BY_LICENSE, HEALTH_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(QUERY_CLAUSES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Query clauses (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Query clauses")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A distinct component/query that serves a specific purpose")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(QUERY_CLAUSES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(BOOLEAN_REFERENCE, VIEW_COORDINATE_KEY)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FIELD_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Field substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Field substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Replacing a placeholder variable in a field with a specific value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FIELD_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(BOOLEAN_SUBSTITUTION, CONCEPT_SUBSTITUTION, FLOAT_SUBSTITUTION, INSTANT_SUBSTITUTION)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TAXONOMY_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Taxonomy operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Taxonomy operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("An operator or set of operations applied within the context of a taxonomy")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TAXONOMY_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(LOGICALLY_EQUIVALENT_TO)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IMMUTABLECOORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("ImmutableCoordinate Properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("ImmutableCoordinate properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A set of values or data representing a point in space that one established cannot be changed?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IMMUTABLECOORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ALLOWED_STATES_FOR_STAMP_COORDINATE, AUTHORS_FOR_STAMP_COORDINATE, MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE, MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE, MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE, MODULES_FOR_STAMP_COORDINATE, AUTHOR_FOR_EDIT_COORDINATE, DEFAULT_MODULE_FOR_EDIT_COORDINATE, DESTINATION_MODULE_FOR_EDIT_COORDINATE, MODULE_OPTIONS_FOR_EDIT_COORDINATE, PATH_OPTIONS_FOR_EDIT_CORDINATE, DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE, DIGRAPH_FOR_LOGIC_COORDINATE, ROOT_FOR_LOGIC_COORDINATE, DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE, LANGUAGE_NID_FOR_LANGUAGE_COORDINATE, LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE, MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE, PATH_FOR_PATH_COORDINATE, PATH_ORIGINS_FOR_STAMP_PATH, VERTEX_SORT, VERTEX_STATE_SET, STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE, INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE, CLASSIFIER_FOR_LOGIC_COORDINATE, POSITION_ON_PATH)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PURPOSE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Purpose")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Purpose")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The reason for which a Tinkar value in a pattern was created or for which it exist.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PURPOSE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ACTION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Action properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Action properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Attributes of an action object")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ACTION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(CONCEPT_CONSTRAINTS, CONCEPT_TO_FIND, ROLE_TYPE_TO_ADD, CONDITIONAL_TRIGGERS)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LITERAL_VALUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Literal value (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Literal value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Fixed Value/Constant?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LITERAL_VALUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(BOOLEAN_LITERAL, FLOAT_LITERAL, INSTANT_LITERAL)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIALECT_ASSEMBLAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dialect")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Specifies the dialect of the language.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIALECT_ASSEMBLAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(CZECH_DIALECT, ENGLISH_DIALECT_ASSEMBLAGE, FRENCH_DIALECT, IRISH_DIALECT, KOREAN_DIALECT, POLISH_DIALECT, RUSSIAN_DIALECT)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DYNAMIC_COLUMN_DATA_TYPES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dynamic column data types (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Dynamic column data types")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Data storage system where the structure of the data can be altered or extended dynamically?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DYNAMIC_COLUMN_DATA_TYPES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ARRAY, BOOLEAN, BYTE_ARRAY, DOUBLE, FLOAT, LONG, SIGNED_INTEGER, STRING, UUID_DATA_TYPE)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        // TODO: Get coordinates to work via Komet's KometTerm
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(POSITION_ON_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(POSITION_ON_PATH.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text(POSITION_ON_PATH.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(POSITION_ON_PATH.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(POSITION_ON_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Stated assemblage")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred assemblage")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Concepts to classify")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CLASSIFIER_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(CLASSIFIER_FOR_LOGIC_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Classifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(CLASSIFIER_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CLASSIFIER_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Dialect order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SOLOR_OVERLAY_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("SOLOR overlay module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("SOLOR overlay module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(SOLOR_OVERLAY_MODULE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SOLOR_OVERLAY_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SOLOR_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("SOLOR module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("SOLOR module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(SOLOR_MODULE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SOLOR_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TRANSITIVE_PROPERTY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Transitive Feature (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Transitive Feature")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(TRANSITIVE_PROPERTY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TRANSITIVE_PROPERTY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFLEXIVE_PROPERTY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Reflexive Feature (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Reflexive Feature")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(REFLEXIVE_PROPERTY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFLEXIVE_PROPERTY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LATERALITY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Laterality (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Laterality")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(LATERALITY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LATERALITY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(HAS_ACTIVE_INGREDIENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Has Active Ingredient (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Has Active Ingredient")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(HAS_ACTIVE_INGREDIENT.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(HAS_ACTIVE_INGREDIENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(HAS_DOSE_FORM))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Has Dose Form (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Has Dose Form")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(HAS_DOSE_FORM.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(HAS_DOSE_FORM.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UNMODELED_ROLE_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Unmodeled role concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Unmodeled role concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(UNMODELED_ROLE_CONCEPT.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UNMODELED_ROLE_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DYNAMIC_REFERENCED_COMPONENT_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dynamic referenced component restriction (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Dynamic referenced component restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(DYNAMIC_REFERENCED_COMPONENT_RESTRICTION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DYNAMIC_REFERENCED_COMPONENT_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EXISTENTIAL_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Existential restriction")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Existential restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Existential restrictions describe objects that participate in at least one relationship along a specified property to objects of a specified class.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EXISTENTIAL_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(ROLE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INTRINSIC_ROLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Intrinsic role (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Intrinsic role")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(INTRINSIC_ROLE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INTRINSIC_ROLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PROPERTY_PATTERN_IMPLICATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Property pattern implication (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Property pattern implication")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(PROPERTY_PATTERN_IMPLICATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PROPERTY_PATTERN_IMPLICATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SNOROCKET_CLASSIFIER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("SnoRocket classifier (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("SnoRocket classifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(SNOROCKET_CLASSIFIER.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SNOROCKET_CLASSIFIER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PROPERTY_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Property set (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Property set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(PROPERTY_SET.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PROPERTY_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FEATURE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Feature (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Feature")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(FEATURE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FEATURE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EXAMPLE_UCUM_UNITS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Example UCUM Units (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Example UCUM Units")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The Unified Code for Units of Measure (UCUM) is a code system intended to " +
                                "include all units of measures being contemporarily used in international science, " +
                                "engineering, and business. (www.unitsofmeasure.org) This field contains example units " +
                                "of measures for this term expressed as UCUM units.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EXAMPLE_UCUM_UNITS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PHENOMENON))
                .attach(new StatedAxiom()
                        .isA(PHENOMENON)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INFERRED_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inferred Definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The relationships/axioms of a concept that have been inferred")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INFERRED_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IDENTIFIER_VALUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Identifier Value (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Identifier Value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The literal string value identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IDENTIFIER_VALUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MAXIMUM_VALUE_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Maximum Value Operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Maximum Value Operator; Maximum Domain Operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The Value Operator assigned to the Maximum Value in a Range")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MAXIMUM_VALUE_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MINIMUM_VALUE_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Minimum Value Operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Minimum Value Operator; Minimum Domain Operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The Value Operator assigned to the Minimum Value in a Range")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MINIMUM_VALUE_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCE_RANGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Reference Range (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Value Range")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The range of values specific to a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCE_RANGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(REFERENCE_RANGE_MAXIMUM, REFERENCE_RANGE_MINIMUM)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCE_RANGE_MAXIMUM))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Reference Range Maximum (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Maximum Value; Max Value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The highest possible value for a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCE_RANGE_MAXIMUM.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(REFERENCE_RANGE))
                .attach(new StatedAxiom()
                        .isA(REFERENCE_RANGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCE_RANGE_MINIMUM))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Reference Range Minimum (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Minimum Value; Min Value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The lowest possible value for a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCE_RANGE_MINIMUM.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(REFERENCE_RANGE))
                .attach(new StatedAxiom()
                        .isA(REFERENCE_RANGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATED_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated Definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Stated Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Relationships/Axioms of a concept that have been explicitly stated and defined")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STATED_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VALUE_CONSTRAINT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Value Constraint (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Value Constraint")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("A component has specific value requirements that needs to be met")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VALUE_CONSTRAINT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VALUE_CONSTRAINT_SOURCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Value Constraint Source (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Value Constraint Source")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("The source organization of that specifies the constraint")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VALUE_CONSTRAINT_SOURCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FEATURE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Feature Type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Feature Type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(FEATURE_TYPE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FEATURE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PROPERTY_SEQUENCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Property Sequence (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Property Sequence")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(PROPERTY_SEQUENCE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PROPERTY_SEQUENCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Stated navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(STATED_NAVIGATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INFERRED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inferred navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(INFERRED_NAVIGATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ANNOTATION_PROPERTY_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Annotation property set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Annotation property set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(ANNOTATION_PROPERTY_SET.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TRANSITIVE_PROPERTY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DATA_PROPERTY_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Data property set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Data property set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(DATA_PROPERTY_SET.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TRANSITIVE_PROPERTY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PROPERTY_SEQUENCE_IMPLICATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Property sequence implication")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("Property sequence implication")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text(PROPERTY_SEQUENCE_IMPLICATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TRANSITIVE_PROPERTY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());
    }

    private void createPatterns(Session session) {
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(INFERRED_NAVIGATION_PATTERN)
                        .meaning(IS_A)
                        .purpose(IS_A)
                        .fieldDefinition(
                                RELATIONSHIP_DESTINATION,
                                IS_A,
                                COMPONENT_ID_SET_FIELD)
                        .fieldDefinition(
                                RELATIONSHIP_ORIGIN,
                                IS_A,
                                COMPONENT_ID_SET_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inferred Navigation Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred Navigation Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Definition definition) -> definition
                        .text("A pattern specifying the origins and destinations for concepts based on the inferred terminological axioms.")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(DESCRIPTION_PATTERN)
                        .meaning(DESCRIPTION_SEMANTIC)
                        .purpose(DESCRIPTION_SEMANTIC)
                        .fieldDefinition(
                                LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION,
                                LANGUAGE,
                                COMPONENT_FIELD)
                        .fieldDefinition(
                                TEXT_FOR_DESCRIPTION,
                                DESCRIPTION,
                                STRING)
                        .fieldDefinition(
                                DESCRIPTION_CASE_SIGNIFICANCE,
                                DESCRIPTION_CASE_SIGNIFICANCE,
                                COMPONENT_FIELD)
                        .fieldDefinition(
                                DESCRIPTION_TYPE,
                                DESCRIPTION_TYPE,
                                COMPONENT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Description Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Definition definition) -> definition
                        .text("Contains all metadata and human readable text that describes the concept")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(STATED_NAVIGATION_PATTERN)
                        .meaning(IS_A)
                        .purpose(IS_A)
                        .fieldDefinition(
                                RELATIONSHIP_DESTINATION,
                                IS_A,
                                COMPONENT_ID_SET_FIELD)
                        .fieldDefinition(
                                RELATIONSHIP_ORIGIN,
                                IS_A,
                                COMPONENT_ID_SET_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated Navigation Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Stated Navigation Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Definition definition) -> definition
                        .text("A pattern specifying the origins and destinations for concepts based on the stated terminological axioms.")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(IDENTIFIER_PATTERN)
                        .meaning(IDENTIFIER_SOURCE)
                        .purpose(IDENTIFIER_SOURCE)
                        .fieldDefinition(
                                IDENTIFIER_SOURCE,
                                IDENTIFIER_SOURCE,
                                COMPONENT_FIELD)
                        .fieldDefinition(
                                IDENTIFIER_VALUE,
                                IDENTIFIER_VALUE,
                                STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Identifier Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Identifier Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Definition definition) -> definition
                        .text("An identifier pattern is used to identity a concept which contains the identifier source and the actual value.")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(US_DIALECT_PATTERN)
                        .meaning(DESCRIPTION_ACCEPTABILITY)
                        .purpose(DESCRIPTION_SEMANTIC)
                        .fieldDefinition(
                                US_ENGLISH_DIALECT,
                                DESCRIPTION_ACCEPTABILITY,
                                COMPONENT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("US Dialect Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("US Dialect Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(GB_DIALECT_PATTERN)
                        .meaning(DESCRIPTION_ACCEPTABILITY)
                        .purpose(DESCRIPTION_SEMANTIC)
                        .fieldDefinition(
                                GB_ENGLISH_DIALECT,
                                DESCRIPTION_ACCEPTABILITY,
                                COMPONENT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("GB Dialect Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("GB Dialect Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Definition definition) -> definition
                        .text("Particular form of language specific form of English language, particular to Great Britain.")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(OWL_AXIOM_SYNTAX_PATTERN)
                        .meaning(AXIOM_SYNTAX)
                        .purpose(EXPRESS_AXIOM_SYNTAX)
                        .fieldDefinition(
                                AXIOM_SYNTAX,
                                EXPRESS_AXIOM_SYNTAX,
                                STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(OWL_AXIOM_SYNTAX_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(EL_PLUS_PLUS_STATED_AXIOMS_PATTERN)
                        .meaning(STATED_DEFINITION)
                        .purpose(LOGICAL_DEFINITION)
                        .fieldDefinition(
                                EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS,
                                LOGICAL_DEFINITION,
                                DITREE_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Stated Axioms Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Stated definition pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(EL_PLUS_PLUS_INFERRED_AXIOMS_PATTERN)
                        .meaning(INFERRED_DEFINITION)
                        .purpose(LOGICAL_DEFINITION)
                        .fieldDefinition(
                                EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS,
                                LOGICAL_DEFINITION,
                                DITREE_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Inferred Axioms Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred definition pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(PATHS_PATTERN)
                        .meaning(PATH)
                        .purpose(MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(PATHS_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(SOLOR_CONCEPT_ASSEMBLAGE)
                        .meaning(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE)
                        .purpose(MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(SOLOR_CONCEPT_ASSEMBLAGE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(STAMP_PATTERN)
                        .meaning(VERSION_PROPERTIES)
                        .purpose(VERSION_PROPERTIES)
                        .fieldDefinition(
                                STATUS_VALUE,
                                STATUS_FOR_VERSION,
                                COMPONENT_FIELD)
                        .fieldDefinition(
                                TIME_FOR_VERSION,
                                TIME_FOR_VERSION,
                                LONG)
                        .fieldDefinition(
                                AUTHOR_FOR_VERSION,
                                AUTHOR_FOR_VERSION,
                                COMPONENT_FIELD)
                        .fieldDefinition(
                                MODULE_FOR_VERSION,
                                MODULE_FOR_VERSION,
                                COMPONENT_FIELD)
                        .fieldDefinition(
                                PATH_FOR_VERSION,
                                PATH_FOR_VERSION,
                                COMPONENT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(STAMP_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(PATH_ORIGINS_PATTERN)
                        .meaning(PATH_ORIGINS)
                        .purpose(PATH_ORIGINS)
                        .fieldDefinition(
                                PATH_CONCEPT,
                                PATH_CONCEPT,
                                COMPONENT_FIELD)
                        .fieldDefinition(
                                PATH_ORIGINS,
                                PATH_ORIGINS,
                                INSTANT_LITERAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path origins pattern (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Path origins pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Definition definition) -> definition
                        .text("Pattern of path origins")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(COMMENT_PATTERN)
                .meaning(COMMENT)
                .purpose(COMMENT)
                .fieldDefinition(
                        COMMENT,
                        COMMENT,
                        STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                .text(COMMENT_PATTERN.description())
                .language(ENGLISH_LANGUAGE)
                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Comment Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TINKAR_BASE_MODEL_COMPONENT_PATTERN)
                        .meaning(STARTER_DATA_AUTHORING)
                        .purpose(MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(TINKAR_BASE_MODEL_COMPONENT_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(KOMET_BASE_MODEL_COMPONENT_PATTERN)
                        .meaning(STARTER_DATA_AUTHORING)
                        .purpose(MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(KOMET_BASE_MODEL_COMPONENT_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new KometBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(VALUE_CONSTRAINT_PATTERN)
                        .meaning(VALUE_CONSTRAINT)
                        .purpose(VALUE_CONSTRAINT)
                        .fieldDefinition(
                                VALUE_CONSTRAINT_SOURCE,
                                VALUE_CONSTRAINT_SOURCE,
                                CONCEPT_FIELD)
                        .fieldDefinition(
                                MINIMUM_VALUE_OPERATOR,
                                CONCRETE_DOMAIN_OPERATOR,
                                    CONCEPT_FIELD)
                        .fieldDefinition(
                                REFERENCE_RANGE_MINIMUM,
                                REFERENCE_RANGE,
                                FLOAT_FIELD)
                        .fieldDefinition(
                                MAXIMUM_VALUE_OPERATOR,
                                CONCRETE_DOMAIN_OPERATOR,
                                COMPONENT_FIELD)
                        .fieldDefinition(
                                REFERENCE_RANGE_MAXIMUM,
                                REFERENCE_RANGE,
                                FLOAT_FIELD)
                        .fieldDefinition(
                                EXAMPLE_UCUM_UNITS,
                                EXAMPLE_UCUM_UNITS,
                                STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Value Constraint Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Value Constraint Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Definition definition) -> definition
                        .text("A pattern specifying value constraint pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());
    }

    private void createPathMembershipSemantics(Session session) {
        createPathMembershipSemantic(session, DEVELOPMENT_PATH);
        createPathMembershipSemantic(session, SANDBOX_PATH);
        createPathMembershipSemantic(session, PRIMORDIAL_PATH);
        createPathMembershipSemantic(session, MASTER_PATH);
    }

    private void createPathMembershipSemantic(Session session, EntityProxy.Concept concept) {
        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(PATHS_PATTERN)
                .reference(concept)
                .fieldValues(MutableList::newEmpty));
    }

    private void addPathOriginSemantics(Session session) {
        addPathOriginSemantic(session, DEVELOPMENT_PATH, SANDBOX_PATH);
        addPathOriginSemantic(session, MASTER_PATH, DEVELOPMENT_PATH);
        addPathOriginSemantic(session, SANDBOX_PATH, PRIMORDIAL_PATH);
    }

    private void addPathOriginSemantic(Session session, EntityProxy.Concept concept, EntityProxy.Concept originPath) {
        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(PATH_ORIGINS_PATTERN)
                .reference(concept)
                .fieldValues(objects -> objects.addAll(Lists.mutable.of(originPath, DateTimeUtil.epochMsToInstant(Long.MAX_VALUE)))));
    }
    
    private USDialect usDialect() {
        return new USDialect().acceptability(PREFERRED);
    }

    private void exportToProtoBuf() {
        try {
            new ExportEntitiesController().export(exportFile).get();
        } catch (ExecutionException | InterruptedException e) {
            LOG.error("Error while exporting.", e);
        }
    }

    public static void main(String[] args){
        TinkarStarterData starterData = new TinkarStarterData(args);
        starterData.init();
        starterData.transform();
        starterData.exportToProtoBuf();
        starterData.cleanup();
    }
}
