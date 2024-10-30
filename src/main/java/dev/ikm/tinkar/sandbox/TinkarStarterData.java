package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.id.PublicIds;
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
import dev.ikm.tinkar.composer.assembler.PatternAssemblerConsumer;
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
import dev.ikm.tinkar.entity.Entity;
import dev.ikm.tinkar.entity.EntityVersion;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.entity.load.LoadEntitiesFromProtobufFile;
import dev.ikm.tinkar.entity.transform.EntityToTinkarSchemaTransformer;
import dev.ikm.tinkar.entity.transform.TinkarSchemaToEntityTransformer;
import dev.ikm.tinkar.schema.TinkarMsg;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.State;
import dev.ikm.tinkar.terms.TinkarTerm;
import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.MutableList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import static dev.ikm.tinkar.terms.TinkarTerm.*;


public class TinkarStarterData {

    private static final Logger LOG = LoggerFactory.getLogger(TinkarStarterData.class.getSimpleName());

    private static File exportDataStore;
    private static File importDataStore;
    private static File exportFile;
    private static Entity<? extends EntityVersion> authoringSTAMP;

    public static void main(String[] args){
        exportDataStore = new File(args[0]);
        exportFile = new File(args[1]);
        importDataStore = new File(args[2]);
        FileUtil.recursiveDelete(exportDataStore);
        FileUtil.recursiveDelete(importDataStore);
        UUIDUtility uuidUtility = new UUIDUtility();

        //Build, export, and shutdown database
        StarterData starterData = new StarterData(exportDataStore, uuidUtility)
                .init()
                .authoringSTAMP(
                        TinkarTerm.ACTIVE_STATE,
                        PrimitiveData.PREMUNDANE_TIME,
                        USER,
                        TinkarTerm.PRIMORDIAL_MODULE,
                        TinkarTerm.PRIMORDIAL_PATH);

        authoringSTAMP = starterData.getAuthoringSTAMP();

        configureConceptsAndPatterns(starterData);
        starterData.build(); //Natively writing data to spined array
        transformAnalysis(uuidUtility); //Isolate and inspect import and export transforms
        exportStarterData();  //exports starter data to pb.zip
        starterData.shutdown();

        //Load exported starter data into clean database
        importStarterData(); //load pb.zip into database
    }

    private static void configureConceptsAndPatterns(StarterData starterData){
        Composer composer = new Composer("ConceptComposer");
        Session session = composer.open(
                State.ACTIVE,
                PrimitiveData.PREMUNDANE_TIME,
                USER,
                TinkarTerm.PRIMORDIAL_MODULE,
                TinkarTerm.PRIMORDIAL_PATH);

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ENGLISH_DIALECT_ASSEMBLAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("English Dialect")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("English dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Specifies the dialect of the English language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ENGLISH_DIALECT_ASSEMBLAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.GB_ENGLISH_DIALECT, TinkarTerm.US_ENGLISH_DIALECT)
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Text comparison measure semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Text comparison")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Text comparison with a focus on semantic meaning involves evaluating the similarity or relatedness between pieces of text based on their underlying meaning rather than just their surface structure.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.TEXT_COMPARISON_MEASURE_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.CASE_INSENSITIVE_EVALUATION, TinkarTerm.CASE_SENSITIVE_EVALUATION)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STARTER_DATA_AUTHORING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(STARTER_DATA_AUTHORING.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Metadata Authoring")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Define necessary minimum viable concepts to use Tinkar Data")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STARTER_DATA_AUTHORING.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.AXIOM_SYNTAX))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(AXIOM_SYNTAX.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Axiom Syntax")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Syntax defining description logic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.AXIOM_SYNTAX.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EXPRESS_AXIOM_SYNTAX))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(TinkarTerm.EXPRESS_AXIOM_SYNTAX.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Express Axiom")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Expressing description logic through syntax")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EXPRESS_AXIOM_SYNTAX.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(AXIOM_SYNTAX))
                .attach(new StatedAxiom()
                        .isA(AXIOM_SYNTAX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ACCEPTABLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Acceptable (foundation metadata concept)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Acceptable")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Specifies that a description is acceptable, but not preferred within a language or dialect.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ACCEPTABLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_ACCEPTABILITY))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_ACCEPTABILITY)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ACTIVE_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Active state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Active")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are active.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ACTIVE_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Allowed states for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Allowed states")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Predefined list of values for STAMP coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.AND))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("And (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("And")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("An operator that typically is employed to combine two conditions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.AND.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ANNOTATION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Annotation type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Annotation type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Metadata about program elements, and annotation types define the structure of these annotations")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ANNOTATION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.COMMENT, TinkarTerm.KOMET_ISSUE)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ANONYMOUS_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Anonymous concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Anonymous concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concepts or entities that do not have a specific, named identity, (defined on-the-fly without a dedicated name)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ANONYMOUS_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCEPT_TYPE))
                .attach(new StatedAxiom()
                        .isA(CONCEPT_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ANY_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Any component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Any component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A general-purpose container to represent any component with generic data structure. Modifiable based on the specific requirements and characteristics of the components.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ANY_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ARRAY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Array (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Array")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Linear data structure")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ARRAY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ARRAY_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Array field (Solor)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Array field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A lexical set of semantically related elements/items")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ARRAY_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Individual or entity who made a particular edit or revision in a document (authoring a specific location or point in the codebase where an edit was made)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.AUTHOR_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Individual or entity who made a specific set of changes or modifications to a codebase/terminology resulting in the creation of a new version or revision")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.AUTHOR_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Authors")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("In individual or an entity responsible for defining or updating the values associated with the STAMP coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.AXIOM_FOCUS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Axiom focus (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Axiom focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A statement or proposition that is assumed to be true without requiring proof, it serves as a foundation principles on which a system or theory is built. Focus refers to the central point of attention or concentration on a specific concept/axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.AXIOM_FOCUS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(COMPONENT_TYPE_FOCUS))
                .attach(new StatedAxiom()
                        .isA(COMPONENT_TYPE_FOCUS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.AXIOM_ORIGIN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Axiom origin (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Axiom origin")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The parent concept for the axiom?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.AXIOM_ORIGIN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.STATED_PREMISE_TYPE, TinkarTerm.INFERRED_PREMISE_TYPE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BOOLEAN_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("True (1) or false (0)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BOOLEAN_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BOOLEAN_LITERAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean literal (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean literal")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("TRUE, FALSE, UNKNOWN")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BOOLEAN_LITERAL.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LITERAL_VALUE))
                .attach(new StatedAxiom()
                        .isA(LITERAL_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BOOLEAN_REFERENCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean reference (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean reference")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Reference(a pointer) to a Boolean object")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BOOLEAN_REFERENCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(QUERY_CLAUSES))
                .attach(new StatedAxiom()
                        .isA(QUERY_CLAUSES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BOOLEAN_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The process of replacing or substituting boolean values or expression in a logical context")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BOOLEAN_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(FIELD_SUBSTITUTION))
                .attach(new StatedAxiom()
                        .isA(FIELD_SUBSTITUTION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BYTE_ARRAY_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Byte array field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Byte array field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("An array of bytes")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BYTE_ARRAY_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CANCELED_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Canceled state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Canceled")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are canceled")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CANCELED_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CASE_INSENSITIVE_EVALUATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Case insensitive evaluation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Ignore case")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Evaluates values regardless of the case")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CASE_INSENSITIVE_EVALUATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .attach(new StatedAxiom()
                        .isA(TEXT_COMPARISON_MEASURE_SEMANTIC)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CASE_SENSITIVE_EVALUATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Case sensitive evaluation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Compare case")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Evaluated based on the case")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CASE_SENSITIVE_EVALUATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .attach(new StatedAxiom()
                        .isA(TEXT_COMPARISON_MEASURE_SEMANTIC)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Case significance concept nid for description (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Case significance")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A field label which captures the case significance for a given concept description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CHINESE_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Chinese language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Chinese language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Chinese language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CHINESE_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CHRONICLE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Chronicle properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Chronicle properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Attributes or characteristic associated with a historical record or an account of events (metadata, timestamps)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CHRONICLE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE, TinkarTerm.VERSION_LIST_FOR_CHRONICLE, TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE, TinkarTerm.UUID_LIST_FOR_COMPONENT)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.COMMENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Comment (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Comment")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A filed label to capture free text information which may be necessary to add or change (concepts, relationships, semantics, etc)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.COMMENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ANNOTATION_TYPE))
                .attach(new StatedAxiom()
                        .isA(ANNOTATION_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.COMPONENT_ID_LIST_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component Id list")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Component Id list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A display field that references an ordered list of Concept IDs.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.COMPONENT_ID_LIST_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.COMPONENT_ID_SET_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component Id set field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Component Id set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A display field that references an unordered list of Concept IDs.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.COMPONENT_ID_SET_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.COMPONENT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Component field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A display field type that references a concept ID.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.COMPONENT_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.COMPONENT_FOR_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component for semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Component for semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.COMPONENT_FOR_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.COMPONENT_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Component Semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Component semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.COMPONENT_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_TYPE))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Component type focus (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Component type focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Focus type of component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.COMPONENT_TYPE_FOCUS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.AXIOM_FOCUS, TinkarTerm.CONCEPT_FOCUS, TinkarTerm.DESCRIPTION_FOCUS)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_CONSTRAINTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept constraints(SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept constraints")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Defined filters for a given concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_CONSTRAINTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ACTION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(ACTION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept details tree table (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept details tree table")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Tree table with concept details")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Field for the human readable description for the given concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_FOCUS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept focus (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_FOCUS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(COMPONENT_TYPE_FOCUS))
                .attach(new StatedAxiom()
                        .isA(COMPONENT_TYPE_FOCUS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_REFERENCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept reference (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept reference")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A field to capture a reference to validate concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_REFERENCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Value to define a given semantic as a concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_TYPE))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Substitution for concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(FIELD_SUBSTITUTION))
                .attach(new StatedAxiom()
                        .isA(FIELD_SUBSTITUTION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_TO_FIND))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept to find (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept to find")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Find concept (if searching on Komet shows us the results 'details and further information?)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_TO_FIND.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ACTION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(ACTION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A field that captures a defined concept label")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ANONYMOUS_CONCEPT, TinkarTerm.PATH_CONCEPT, TinkarTerm.SEMANTIC_FIELD_CONCEPTS)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCEPT_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concept version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Version")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A filed that captures the version of the terminology that it came from")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCEPT_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());



        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Concrete value operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concrete value operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A concept that specifies value operators")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONCRETE_DOMAIN_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.EQUAL_TO, TinkarTerm.GREATER_THAN, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, TinkarTerm.LESS_THAN, TinkarTerm.LESS_THAN_OR_EQUAL_TO, MAXIMUM_VALUE_OPERATOR, MINIMUM_VALUE_OPERATOR)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONDITIONAL_TRIGGERS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Conditional triggers (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Conditional triggers")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Conditional triggers based on actions, reasoner")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONDITIONAL_TRIGGERS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ACTION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(ACTION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CONNECTIVE_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Connective operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Connective operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A field that captures what the operator is (logical connective)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CONNECTIVE_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.AND, TinkarTerm.DEFINITION_ROOT, TinkarTerm.DISJOINT_WITH, TinkarTerm.OR, TinkarTerm.IS_A, TinkarTerm.PART_OF, TinkarTerm.CONCEPT_REFERENCE)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CORELATION_EXPRESSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Correlation expression (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Correlation expression")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A value for Correlation properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CORELATION_EXPRESSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CORRELATION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CORRELATION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CORELATION_REFERENCE_EXPRESSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Correlation reference expression (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Correlation reference expression")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A value for correlation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CORELATION_REFERENCE_EXPRESSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CORRELATION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CORRELATION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CORRELATION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Correlation properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Correlation properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Characteristics or measures that describe the relationship between two or more variables")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CORRELATION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.CORELATION_EXPRESSION, TinkarTerm.CORELATION_REFERENCE_EXPRESSION)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Creative Commons BY license (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Creative Commons BY license")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Creative Commons (CC) licenses are a set of public copyright licenses that enable the free distribution of an otherwise copyrighted work")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CZECH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Czech dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Czech dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Czech dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CZECH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.CZECH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Czech language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Czech language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Czech Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.CZECH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DANISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Danish language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Danish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Danish Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DANISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Default module for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Default module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A value for coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DEFINITION_DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Definition description type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Semantic value describing the description type for the description pattern is a definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DEFINITION_DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DEFINITION_ROOT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Definition root (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Definition root")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DEFINITION_ROOT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Human readable text for a concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DESCRIPTION_ACCEPTABILITY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description acceptability")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description acceptability")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Whether a given human readable text for a concept is permissible")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DESCRIPTION_ACCEPTABILITY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ACCEPTABLE, TinkarTerm.PREFERRED)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_CASE_SENSITIVE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description case sensitive")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Case sensitive")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Assumes the description is dependent on capitalization")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_CASE_SENSITIVE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_CASE_SIGNIFICANCE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description case significance")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description case significance")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Specifies how to handle the description text in terms of case sensitivity")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.DESCRIPTION_CASE_SENSITIVE, TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_CORE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description core type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description core type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Used to mark non-snomed descriptions as one of the core snomed types")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_CORE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description dialect pair (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description dialect pair")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Description dialect pair - linking together dialects with language descriptions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_DIALECT_PAIR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR, TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR)
                        .parents(DESCRIPTION_VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_FOCUS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description focus (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Description focus")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_FOCUS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(COMPONENT_TYPE_FOCUS))
                .attach(new StatedAxiom()
                        .isA(COMPONENT_TYPE_FOCUS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description for dialect/description pair (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description for dialect/description pair")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Linking together dialects with language descriptions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_DIALECT_PAIR))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_DIALECT_PAIR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description initial character case sensitive (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Initial character case insensitive")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Value which designates initial character as sensitive for a given description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_CASE_SIGNIFICANCE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description logic profile for logic coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Logic profile")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description not case sensitive")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Case insensitive")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Value which designate character as not sensitive for a given description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_CASE_SIGNIFICANCE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description semantic")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Purpose and meaning for the description pattern and dialect patterns")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Specifying what type of description it is i.e. is it fully qualified or regular and etc.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.DEFINITION_DESCRIPTION_TYPE, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description type for description (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Linking for each description -> what type it is")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description type preference list for language coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Type order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description version properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description version properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Combination of terms that might be used in a specific context or domain")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION, TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION, TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION, DESCRIPTION_DIALECT_PAIR)
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description-logic profile (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description-logic profile")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_LOGIC_PROFILE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.EL_PLUS_PLUS_PROFILE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Destination module for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Destination module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DEVELOPMENT_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Development module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Development module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Predefines or standard module within a system or application that is specifically designed to support the development phase of a project")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DEVELOPMENT_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MODULE))
                .attach(new StatedAxiom()
                        .isA(MODULE)).attach(new TinkarBaseModel());


/*
        starterData.concept(TinkarTerm.DEVELOPMENT_PATH)
                .fullyQualifiedName("Development path", TinkarTerm.PREFERRED)
                .synonym("Development path", TinkarTerm.PREFERRED)
                .definition("A path that specifies that the components are currently under development", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEVELOPMENT_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(PATH))
                .statedDefinition(List.of(PATH))
                .pathMembership()
                .pathOrigin(TinkarTerm.SANDBOX_PATH)
                .tinkarBaseModelMembership()
                .build();
*/
        // todo
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DEVELOPMENT_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Development path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Development path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A path that specifies that the components are currently under development")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DEVELOPMENT_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH))
                .attach(new StatedAxiom()
                        .isA(PATH)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DIGRAPH_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("DiGraph field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Instant/ DiGraph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A display field that references a di-graph whose edges are ordered pairs of vertices. Each edge can be followed from one vertex to another vertex.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DIGRAPH_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DITREE_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("DiTree field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("DiTree")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A display field that references a graph obtained from an undirected tree by replacing each undirected edge by two directed edges with opposite directions.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DITREE_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dialect for dialect/description pair (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Dialect for dialect/description pair")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Specific dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_DIALECT_PAIR))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_DIALECT_PAIR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Digraph for logic coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Digraph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A value which describes a immutable coordinate property")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DIRECTED_GRAPH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Directed graph (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("NavigationCoordinate/Directed graph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DIRECTED_GRAPH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.EL_PLUS_PLUS_DIGRAPH)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DISJOINT_WITH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Disjoint with (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Disjoint with")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("An operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DISJOINT_WITH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DISPLAY_FIELDS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Display Fields")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Display fields")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Captures the human readable terms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DISPLAY_FIELDS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.COMPONENT_FIELD, TinkarTerm.COMPONENT_ID_LIST_FIELD, TinkarTerm.COMPONENT_ID_SET_FIELD, TinkarTerm.CONCEPT_FIELD, TinkarTerm.DIGRAPH_FIELD, TinkarTerm.DITREE_FIELD, TinkarTerm.FLOAT_FIELD, TinkarTerm.INTEGER_FIELD, TinkarTerm.SEMANTIC_FIELD_TYPE, TinkarTerm.STRING)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DOUBLE_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Double field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Double field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A data value (type of structure for data)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DOUBLE_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DUTCH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dutch language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Dutch language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Dutch language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DUTCH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EL_PROFILE_SET_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("El profile set operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("EL profile set operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("EL profile set operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EL_PROFILE_SET_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.NECESSARY_SET, TinkarTerm.SUFFICIENT_SET)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ terminological axioms")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ terminological axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The set of relationships or axioms has defined by the EL++ Logic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("El++ Inferred Concept Definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ Inferred Concept Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LOGICAL_DEFINITION))
                .attach(new StatedAxiom()
                        .isA(LOGICAL_DEFINITION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Inferred terminological axioms")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ Inferred terminological axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.NECESSARY_SET, TinkarTerm.SUFFICIENT_SET, TinkarTerm.INCLUSION_SET, ROLE, TinkarTerm.ROLE_GROUP)
                        .parents(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Stated Concept Definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ Stated Concept Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LOGICAL_DEFINITION))
                .attach(new StatedAxiom()
                        .isA(LOGICAL_DEFINITION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Stated terminological axioms")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ Stated terminological axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.NECESSARY_SET, TinkarTerm.SUFFICIENT_SET, TinkarTerm.INCLUSION_SET, ROLE, TinkarTerm.ROLE_GROUP)
                        .parents(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EL_PLUS_PLUS_DIGRAPH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ digraph (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("EL++ digraph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The directed graph that results from classifying a set of EL++ axioms")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EL_PLUS_PLUS_DIGRAPH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIRECTED_GRAPH))
                .attach(new StatedAxiom()
                        .isA(DIRECTED_GRAPH)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EL_PLUS_PLUS_PROFILE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ logic profile (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("EL ++ logic profile")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("EL ++ profile")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EL_PLUS_PLUS_PROFILE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_LOGIC_PROFILE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_LOGIC_PROFILE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ENGLISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("English Language")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("English language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Value for description language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ENGLISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EQUAL_TO))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Equal to (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Equal to")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \"=\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EQUAL_TO.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EXACT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Exact (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Exact")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Source and target are semantic or exact lexical match")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EXACT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(GROUPING))
                .attach(new StatedAxiom()
                        .isA(GROUPING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EXTENDED_DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Extended description type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Extended description type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Used to store non-snomed description types when other terminologies are imported")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EXTENDED_DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Extended relationship type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Extended relationship type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Used to store non-snomed relationship types when other terminologies are imported- especially when a relationship is mapped onto a snomed relationship type (such as isa)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.FLOAT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Float field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Represents values as high-precision fractional values.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.FLOAT_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.FLOAT_LITERAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float literal (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Float literal")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Numbers with decimal point or an exponential part")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.FLOAT_LITERAL.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LITERAL_VALUE))
                .attach(new StatedAxiom()
                        .isA(LITERAL_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.FLOAT_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Float substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.FLOAT_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(FIELD_SUBSTITUTION))
                .attach(new StatedAxiom()
                        .isA(FIELD_SUBSTITUTION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.FRENCH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("French dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("French dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("French dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.FRENCH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.FRENCH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("French Language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("French language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("French Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.FRENCH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Fully qualified name description type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Fully qualified name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Fully qualified name is a description that uniquely identifies and differentiates it from other concepts with similar descriptions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.GB_ENGLISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Great Britain English dialect")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("GB English dialect / GB English")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Great Britain: English Language reference set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.GB_ENGLISH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ENGLISH_DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(ENGLISH_DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.GERMAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("German Language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("German language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("German Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.GERMAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.GREATER_THAN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Greater than (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Greater than")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \">\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.GREATER_THAN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.GREATER_THAN_OR_EQUAL_TO))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Greater than or equal to (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Greater than or equal to")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \">=\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.GREATER_THAN_OR_EQUAL_TO.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(GROUPING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Health concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Health concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(GROUPING.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.EXACT, TinkarTerm.PARTIAL)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.HEALTH_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Health concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Health concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.HEALTH_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.IDENTIFIER_SOURCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Identifier Source")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Identifier source")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("An identifier used to label the identity of a unique component.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.IDENTIFIER_SOURCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.INCLUSION_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inclusion set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Inclusion set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A set of relationships that indicate something is has an inclusion. Not necessarily or sufficient but inclusive.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.INCLUSION_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.INACTIVE_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inactive state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Inactive")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are no longer active")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.INACTIVE_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.INFERRED_PREMISE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inferred premise type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred relationship / Inferred")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The axiom view following the application of the reasoner")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.INFERRED_PREMISE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(AXIOM_ORIGIN))
                .attach(new StatedAxiom()
                        .isA(AXIOM_ORIGIN)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.INSTANT_LITERAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Instant literal (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Instant literal")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("May refer to a specific point in time which is often represented by a date or time value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.INSTANT_LITERAL.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LITERAL_VALUE))
                .attach(new StatedAxiom()
                        .isA(LITERAL_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.INSTANT_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Instant substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Instant substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Substitution of instant literal?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.INSTANT_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(FIELD_SUBSTITUTION))
                .attach(new StatedAxiom()
                        .isA(FIELD_SUBSTITUTION)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.INTEGER_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Integer Field")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Integer field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data type that represents some range of mathematical integers")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.INTEGER_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.INVERSE_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inverse name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Inverse name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("This is the extended description type that maye be attached to a description within a concept that defines as Association refex to signify that the referenced description  is the inverse of the association name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.INVERSE_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.INVERSE_TREE_LIST))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inverse tree list (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Inverse tree list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Inverse tree list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.INVERSE_TREE_LIST.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TREE_AMALGAM_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TREE_AMALGAM_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.IRISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Irish dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Irish dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Irish dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.IRISH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.IRISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Irish language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Irish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Irish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.IRISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.IS_A))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Is-a")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Is a")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Designates the parent child relationship")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.IS_A.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.IS_A_INFERRED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Is-a inferred navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Is-a inferred navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Designates the parent child relationship following the application of the reasoner")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.IS_A_INFERRED_NAVIGATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(NAVIGATION))
                .attach(new StatedAxiom()
                        .isA(NAVIGATION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.IS_A_STATED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Is-a stated navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Is-a stated navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Designates the parent child relationship as authored")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.IS_A_STATED_NAVIGATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(NAVIGATION))
                .attach(new StatedAxiom()
                        .isA(NAVIGATION)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ITALIAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Italian Language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Italian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Italian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ITALIAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.KOMET_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("KOMET module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("KOMET module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Komet specific values?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.KOMET_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MODULE))
                .attach(new StatedAxiom()
                        .isA(MODULE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.KOMET_USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("KOMET user (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("KOMET user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Authorized to author, edit and/or view in Komet")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.KOMET_USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.KOMET_USER_LIST))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("KOMET user list (SOLOR")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("KOMET user list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Inventory of authorized komet users")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.KOMET_USER_LIST.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.KOMET_ISSUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Komet issue (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Komet issue")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Komet being the 'annotation type' - specified type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.KOMET_ISSUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ANNOTATION_TYPE))
                .attach(new StatedAxiom()
                        .isA(ANNOTATION_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.KOREAN_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Korean dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Korean dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Korean dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.KOREAN_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.STANDARD_KOREAN_DIALECT)
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.KOREAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Korean Language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Korean language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Korean language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.KOREAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Specifies the language of the description text.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ENGLISH_LANGUAGE, TinkarTerm.SPANISH_LANGUAGE)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language concept nid for description (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Language for description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Captures the language code for a description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LANGUAGE_COORDINATE_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language coordinate name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Language coordinate name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LANGUAGE_COORDINATE_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LANGUAGE_COORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language coordinate properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Language coordinate properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Spatial representation of language, attributes or language coordinates, programming language metadata?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LANGUAGE_COORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.LANGUAGE_COORDINATE_NAME, DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language nid for language coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Language nid")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Language specification for language coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LESS_THAN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Less than (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Less than")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \"<\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LESS_THAN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LESS_THAN_OR_EQUAL_TO))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Less than or equal to (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Less than or equal to")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A concept indicating the operator \"<=\"")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LESS_THAN_OR_EQUAL_TO.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONCRETE_DOMAIN_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LITHUANIAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Lithuanian language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Lithuanian Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Lithuanian Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LITHUANIAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LOGIC_COORDINATE_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logic coordinate name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Logic coordinate name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LOGIC_COORDINATE_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LOGIC_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(LOGIC_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGIC_COORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logic coordinate properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Logic coordinate properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Structural characteristics of logical elements, Attributes of Logical coordinates, Mathematical Representation of logical relationships ?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGIC_COORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.LOGIC_COORDINATE_NAME)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LOGICAL_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logical Definition")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Logical Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The semantic value describing the purpose of the stated and inferred terminological axioms.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LOGICAL_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LOGICAL_EXPRESSION_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logical expression field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Logical expression field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LOGICAL_EXPRESSION_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logical expression semantic  (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Logical expression semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_TYPE))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_TYPE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LOGICALLY_EQUIVALENT_TO))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logically equivalent to (Solor)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Logically equivalent to")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("An operator for the reasoner to determine the equivalence")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LOGICALLY_EQUIVALENT_TO.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TAXONOMY_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(TAXONOMY_OPERATOR)).attach(new TinkarBaseModel());


        /*starterData.concept(TinkarTerm.MASTER_PATH)
                .fullyQualifiedName("Master path", TinkarTerm.PREFERRED)
                .synonym("Master path", TinkarTerm.PREFERRED)
                .definition("A default path for components", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MASTER_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(PATH))
                .statedDefinition(List.of(PATH))
                .pathMembership()
                .pathOrigin(TinkarTerm.DEVELOPMENT_PATH)
                .tinkarBaseModelMembership()
                .build();*/

        //todo implement pathmembership & origin
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MASTER_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Master path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Master path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A default path for components")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MASTER_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH))
                .attach(new StatedAxiom()
                        .isA(PATH)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MEANING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Meaning")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Meaning")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The interpretation or explanation field for a pattern/semantics")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Membership semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Membership semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MEMBERSHIP_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(SEMANTIC_TYPE))
                .attach(new StatedAxiom()
                        .isA(SEMANTIC_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODEL_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Model concept")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Model concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(" ")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODEL_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.TINKAR_MODEL_CONCEPT)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.TINKAR_MODEL_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Tinkar Model concept")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Tinkar Model concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(" ")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.TINKAR_MODEL_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(AXIOM_ORIGIN, CONCRETE_DOMAIN_OPERATOR, TinkarTerm.DESCRIPTION, DESCRIPTION_ACCEPTABILITY,
                                DESCRIPTION_CASE_SIGNIFICANCE, TinkarTerm.DESCRIPTION_SEMANTIC, DESCRIPTION_TYPE,
                                DIALECT_ASSEMBLAGE, DISPLAY_FIELDS, EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS,
                                IDENTIFIER_SOURCE, IDENTIFIER_VALUE, TinkarTerm.INFERRED_DEFINITION, TinkarTerm.IS_A,
                                LANGUAGE, LOGICAL_DEFINITION, MEANING, TinkarTerm.PURPOSE,
                                PHENOMENON, TinkarTerm.RELATIONSHIP_DESTINATION, TinkarTerm.RELATIONSHIP_ORIGIN,
                                REFERENCE_RANGE, STATED_DEFINITION, TinkarTerm.TEXT_FOR_DESCRIPTION, VALUE_CONSTRAINT,
                                VALUE_CONSTRAINT_SOURCE, AXIOM_SYNTAX)
                        .parents(MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.PRIMORDIAL_MODULE)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module exclusion set for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Module exclusions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Module exclusion set for stamp coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODULE_FOR_USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module for user (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Module for user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("User preference for Module?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODULE_FOR_USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODULE_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Module Version")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODULE_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module options for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Module options")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Coordinate edit options for Module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module preference list for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Module Preference list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Module preference list for stamp coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module preference list for language coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Module nids")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Module preference list for language coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Module preference order for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Module order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Module preference order for stamp coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.MODULES_FOR_STAMP_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Modules for stamp coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Modules")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Stamp coordinate modules")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.MODULES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.NAVIGATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.NAVIGATION_CONCEPT_SET, TinkarTerm.NAVIGATION_VERTEX)
                        .parents(PURPOSE))
                .attach(new StatedAxiom()
                        .isA(PURPOSE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.NAVIGATION_CONCEPT_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Navigation concept set (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Navigation set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Navigating sets of concepts?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.NAVIGATION_CONCEPT_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(NAVIGATION))
                .attach(new StatedAxiom()
                        .isA(NAVIGATION)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.NAVIGATION_VERTEX))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Navigation vertex (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Navigation vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Navigation vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.NAVIGATION_VERTEX.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(NAVIGATION))
                .attach(new StatedAxiom()
                        .isA(NAVIGATION)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Necessary but not sufficient concept definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Necessary but not sufficient concept definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Not sufficiently defined by necessary conditions definition status")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.NECESSARY_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Necessary set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Necessary set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A set of relationships that is always true of a concept.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.NECESSARY_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());



        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.NOT_APPLICABLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Not Applicable (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Not applicable")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Not available")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.NOT_APPLICABLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_CASE_SIGNIFICANCE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_CASE_SIGNIFICANCE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.OBJECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Object (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Object")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("An encapsulation of data together with procedures")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.OBJECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(STATUS_VALUE, TinkarTerm.DESCRIPTION, TinkarTerm.NID, TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANY_COMPONENT, TinkarTerm.UNINITIALIZED_COMPONENT, TinkarTerm.SANDBOX_COMPONENT, MODULE, PATH, OBJECT_PROPERTIES, HAS_ACTIVE_INGREDIENT, HAS_DOSE_FORM, LATERALITY)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.OBJECT_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Object Properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Object properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Objects are instances of classes, the properties describe the data or attributes that an object can have")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.OBJECT_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ACTION_PROPERTIES, CHRONICLE_PROPERTIES, VERSION_PROPERTIES, IMMUTABLECOORDINATE_PROPERTIES, LANGUAGE_COORDINATE_PROPERTIES, LOGIC_COORDINATE_PROPERTIES, TinkarTerm.PATH_COORDINATE_PROPERTIES, TinkarTerm.SEMANTIC_PROPERTIES, TREE_AMALGAM_PROPERTIES, CORRELATION_PROPERTIES, TRANSITIVE_PROPERTY, REFLEXIVE_PROPERTY)
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.OR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Or (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Or")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.OR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Order for axiom attachments (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Axiom attachment order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Order in which axioms are attached")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Order for concept attachments  (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concept attachment order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Order in which concepts are attached")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Order for description attachments (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description attachment order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Order in which descriptions are attached")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PART_OF))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Part of (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Part of")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Part of an attribute")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PART_OF.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONNECTIVE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(CONNECTIVE_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PARTIAL))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Partial (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Partial")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Exists in/ Inclusion of ?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PARTIAL.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(GROUPING))
                .attach(new StatedAxiom()
                        .isA(GROUPING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A set of assets under version control that can be managed distinctly from other assets. Paths “branch” from other paths when established, and can be “merged” with other paths as well.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.DEVELOPMENT_PATH, TinkarTerm.MASTER_PATH, TinkarTerm.PRIMORDIAL_PATH, TinkarTerm.SANDBOX_PATH)
                        .parents(TinkarTerm.ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Path concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCEPT_TYPE))
                .attach(new StatedAxiom()
                        .isA(CONCEPT_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_COORDINATE_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path coordinate name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path coordinate name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Path coordinate name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_COORDINATE_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.PATH_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path coordinate properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path coordinate properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Character or attribute of coordinates referring to a series of connected points, that form a shape or trajectory")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_COORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.PATH_COORDINATE_NAME, TinkarTerm.PATH_ORIGINS)
                        .parents(TinkarTerm.OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.OBJECT_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_FOR_PATH_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path for path coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Path for path coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_FOR_PATH_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_FOR_USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path for user (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path for user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Path for user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_FOR_USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path for version")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Version path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.VERSION_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path options for edit coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path options")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Path options for edit coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_ORIGINS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path origins (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path origins")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Path origins")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_ORIGINS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.PATH_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Path origins for stamp path (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Path origins")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Path origins for stamp path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PROMOTION_PATH_FOR_EDIT_CORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Promotion Path for Edit Coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Promotion Path for Edit Coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Promotion Path for Edit Coordinate")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PROMOTION_PATH_FOR_EDIT_CORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.PATH_COORDINATE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PHENOMENON))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Phenomenon")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Phenomenon")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A unique thought, fact, or circumstance")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PHENOMENON.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(EXAMPLE_UCUM_UNITS)
                        .parents(TinkarTerm.TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.POLISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Polish dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Polish dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Polish Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.POLISH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.POLISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Polish Language (Language)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Polish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Polish Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.POLISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PREFERRED))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Preferred (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Preferred")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Preferred( Foundation metadata concept)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PREFERRED.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_ACCEPTABILITY))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_ACCEPTABILITY)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PRESENTATION_UNIT_DIFFERENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Presentation unit different (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Presentation unit different")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Unit difference")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PRESENTATION_UNIT_DIFFERENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Primordial UUID for chronicle (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Primordial UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Primordial UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.CHRONICLE_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PRIMORDIAL_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Primordial module")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Primordial module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                . attach((Definition definition) -> definition
                        .text(" ")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PRIMORDIAL_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MODULE))
                .attach(new StatedAxiom()
                        .isA(MODULE)).attach(new TinkarBaseModel());


      /*  starterData.concept(TinkarTerm.PRIMORDIAL_PATH)
                .fullyQualifiedName("Primordial path", TinkarTerm.PREFERRED)
                .synonym("Primordial path", TinkarTerm.PREFERRED)
                .definition("", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH))
                .statedDefinition(List.of(TinkarTerm.PATH))
                .pathMembership()
                .tinkarBaseModelMembership()
                .build();*/

        //todo implement pathmembership
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PRIMORDIAL_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Primordial path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Primordial path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(" ")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PRIMORDIAL_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH))
                .attach(new StatedAxiom()
                        .isA(PATH)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.PRIMORDIAL_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Primordial state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Primordial")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that have not yet been released and exist in their most basic form.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PRIMORDIAL_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Referenced component nid for semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Referenced component id")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Component id Referenced")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SEMANTIC_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Referenced component subtype restriction (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Referenced component subtype restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Stores the (optional) referenced component type sub restriction selection which will be used by the validator to check the user input for the referenced component when creating an instance of a dynamic field.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.ROLE_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Referenced component type restriction (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Referenced component type restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Stores the (Optional) referenced component type restriction selection which will be used by the validator to check the user input for the referenced component when creating an instance of a dynamic field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.ROLE_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Regular name description type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Regular name description type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("There may be descriptions/synonyms marked as “regular.”")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.RELATIONSHIP_DESTINATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Relationship destination")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Relationship destination")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Signifies path to child concepts which are more specific than the Tinkar term")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.RELATIONSHIP_DESTINATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.RELATIONSHIP_ORIGIN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Relationship origin")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Relationship origin")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Signifies path to parent concepts which are more general than the Tinkar term")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.RELATIONSHIP_ORIGIN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ROLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Role")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Is an abstract representation of a high-level role for a therapeutic medicinal product; the concepts are not intended to describe a detailed indication for therapeutic use nor imply that therapeutic use is appropriate in all clinical situations.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ROLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ROLE_TYPE, ROLE_OPERATOR, TinkarTerm.ROLE_RESTRICTION)
                        .parents(TinkarTerm.ROLE_GROUP, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.ROLE_GROUP, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ROLE_GROUP))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role group")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Role group")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("An association between a set of attribute or axiom value pairs that causes them to be considered together within a concept definition or post coordinated expression.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ROLE_GROUP.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(ROLE)
                        .parents(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS) );


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ROLE_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role operator")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Role operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concept that is used to describe universal vs existential restrictions.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ROLE_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.UNIVERSAL_RESTRICTION, EXISTENTIAL_RESTRICTION)
                        .parents(ROLE))
                .attach(new StatedAxiom()
                        .isA(ROLE) );


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ROLE_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(ROLE_RESTRICTION.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Role value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Role restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ROLE_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE))
                .attach(new StatedAxiom()
                        .isA(ROLE) );


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ROLE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role type")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Role type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Refers to a concept that represents a particular kind of relationship that can exist between two entities. It defines the specific function or responsibility that one entity plays in relation to another.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ROLE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE))
                .attach(new StatedAxiom()
                        .isA(ROLE) );


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ROLE_TYPE_TO_ADD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Role type to add (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Role type to add")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Action - add role type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ROLE_TYPE_TO_ADD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ACTION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(ACTION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Root for logic coordinate (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Root")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Logic coordinate root")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.RUSSIAN_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Russian dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Russian dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Russian Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.RUSSIAN_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.RUSSIAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Russian language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Russian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Russian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.RUSSIAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SANDBOX_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Sandbox component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Sandbox component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SANDBOX_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.SANDBOX_MODULE, TinkarTerm.SANDBOX_PATH)
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());


/*
        starterData.concept(TinkarTerm.SANDBOX_MODULE)
                .fullyQualifiedName("Sandbox module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sandbox module", TinkarTerm.PREFERRED)
                .definition("Sandbox module", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_MODULE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.SANDBOX_PATH_MODULE), List.of(TinkarTerm.MODULE, TinkarTerm.SANDBOX_COMPONENT))
                .statedDefinition(List.of(TinkarTerm.MODULE, TinkarTerm.SANDBOX_COMPONENT))
                .tinkarBaseModelMembership()
                .build();
*/

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SANDBOX_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Sandbox module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Sandbox module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SANDBOX_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.SANDBOX_PATH_MODULE)
                        .parents(MODULE, TinkarTerm.SANDBOX_COMPONENT))
                .attach(new StatedAxiom()
                        .isA(MODULE, TinkarTerm.SANDBOX_COMPONENT)).attach(new TinkarBaseModel());


        //TODO how to implement pathOrigin? is this important?
        /*starterData.concept(TinkarTerm.SANDBOX_PATH)
                .fullyQualifiedName("Sandbox path", TinkarTerm.PREFERRED)
                .synonym("Sandbox path", TinkarTerm.PREFERRED)
                .definition("A path for components under testing.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH))
                .statedDefinition(List.of(TinkarTerm.PATH))
                .pathMembership()
                .pathOrigin(TinkarTerm.PRIMORDIAL_PATH)
                .tinkarBaseModelMembership()
                .build();*/

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SANDBOX_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox path")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Sandbox path")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A path for components under testing.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SANDBOX_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PATH))
                .attach(new StatedAxiom()
                        .isA(PATH)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SANDBOX_PATH_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox path module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Sandbox Path module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Sandbox path module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SANDBOX_PATH_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SANDBOX_MODULE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SANDBOX_MODULE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_FIELD_CONCEPTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field concepts (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic field concepts")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Semantic field concepts")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_FIELD_CONCEPTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CONCEPT_TYPE))
                .attach(new StatedAxiom()
                        .isA(CONCEPT_TYPE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_FIELD_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Field name")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Field name - semantics")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_FIELD_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SEMANTIC_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_FIELD_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic field type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("List of fields-  semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_FIELD_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DISPLAY_FIELDS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The attributes or characteristics of a concept, term, or element that convey meaning or semantics in a given context")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC, TinkarTerm.COMPONENT_FOR_SEMANTIC, TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC, TinkarTerm.SEMANTIC_FIELD_NAME)
                        .parents(TinkarTerm.OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.OBJECT_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SEMANTIC_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Type- semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SEMANTIC_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.COMPONENT_SEMANTIC, TinkarTerm.CONCEPT_SEMANTIC, TinkarTerm.DESCRIPTION_SEMANTIC, TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC, MEMBERSHIP_SEMANTIC)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SIGNED_INTEGER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Signed integer (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Signed integer")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Signed integer (Foundation metadata concept)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SIGNED_INTEGER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SPANISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Spanish language")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Spanish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Value for the description language dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SPANISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.LANGUAGE)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STANDARD_KOREAN_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Standard Korean dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Standard Korean Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Standard")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STANDARD_KOREAN_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.KOREAN_DIALECT))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.KOREAN_DIALECT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STATED_PREMISE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated premise type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Stated")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Stated relationship")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STATED_PREMISE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(AXIOM_ORIGIN))
                .attach(new StatedAxiom()
                        .isA(AXIOM_ORIGIN)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STATUS_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Status for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Status for version")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Version status?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STATUS_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.VERSION_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATUS_VALUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Status value")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Status")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The status of the STAMP Coordinate(Active, Cancelled, Inactive, Primordial)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STATUS_VALUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ACTIVE_STATE, TinkarTerm.CANCELED_STATE, TinkarTerm.INACTIVE_STATE, TinkarTerm.PRIMORDIAL_STATE, TinkarTerm.WITHDRAWN_STATE)
                        .parents(TinkarTerm.ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.ROOT_VERTEX)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("String")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("String")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A sequence of characters, either as a literal constant or as a variable. Strings could be used to represent terms from code systems or URLs, textual definitions, etc.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STRING.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DISPLAY_FIELDS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient concept definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Sufficient concept definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concept definition - Sufficient")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient concept definition operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Sufficient concept definition operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concept definition operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION, TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SUFFICIENT_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Sufficient set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A set of relationships that differentiate a concept and its subtypes from all other concepts. A concept that contains at least one set of necessary and sufficient conditions is considered defined.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SUFFICIENT_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SWEDISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Swedish language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Swedish language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Swedish Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SWEDISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.TEXT_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Text for description")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Text")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Captures the human readable text for a description in Komet")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.TEXT_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.TIME_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Time for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Time for version")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Version time")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.TIME_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TREE_AMALGAM_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Tree amalgam properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Tree amalgam properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data structure that consists of nodes connected by edges (a mixture or blend of different elements)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TREE_AMALGAM_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.INVERSE_TREE_LIST, TinkarTerm.TREE_LIST)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.TREE_LIST))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Tree list (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Tree list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("List - Tree")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.TREE_LIST.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TREE_AMALGAM_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TREE_AMALGAM_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.US_ENGLISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("United States of America English dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("US English dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("USA -english dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.US_ENGLISH_DIALECT.asUuidArray()[0].toString()))
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("United States English Nursing Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Nursing Dialect -US English")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(US_NURSING_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(US_ENGLISH_DIALECT))
                .attach(new StatedAxiom()
                        .isA(US_ENGLISH_DIALECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UUID_DATA_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID data type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("UUID data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Distinction of data type of UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UUID_DATA_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UUID_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("UUID field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Universally unique identifier field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UUID_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UUID_LIST_FOR_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID list for component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("UUIDs")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("UUIDs")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UUID_LIST_FOR_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UNCATEGORIZED_PHENOMENON))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Uncategorized phenomenon (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Uncategorized phenomenon")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Unknown")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UNCATEGORIZED_PHENOMENON.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PHENOMENON))
                .attach(new StatedAxiom()
                        .isA(PHENOMENON)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UNINITIALIZED_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Uninitialized Component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Uninitialized")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Not initialized component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UNINITIALIZED_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UNIVERSAL_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Universal Restriction")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Universal Restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Universal restrictions constrain the relationships along a given property to concepts that are members of a specific class.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UNIVERSAL_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(ROLE_OPERATOR)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UNIVERSALLY_UNIQUE_IDENTIFIER")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A universally unique identifier that uniquely represents a concept in Tinkar")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IDENTIFIER_SOURCE))
                .attach(new StatedAxiom()
                        .isA(IDENTIFIER_SOURCE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.KOMET_USER, TinkarTerm.KOMET_USER_LIST, TinkarTerm.MODULE_FOR_USER, TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS, TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS, TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS, TinkarTerm.PATH_FOR_USER, STARTER_DATA_AUTHORING)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VERSION_LIST_FOR_CHRONICLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Version list for chronicle (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Versions")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Chronicle version list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VERSION_LIST_FOR_CHRONICLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VERSION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Version Properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Version properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VERSION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.AUTHOR_FOR_VERSION, TinkarTerm.MODULE_FOR_VERSION, TinkarTerm.PATH_FOR_VERSION, TinkarTerm.STATUS_FOR_VERSION, TinkarTerm.TIME_FOR_VERSION, DESCRIPTION_VERSION_PROPERTIES)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VERTEX_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Field for Vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VERTEX_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VERTEX_STATE_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex state set (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Vertex states")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Vertex states")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VERTEX_STATE_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VERTEX_SORT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex sort (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Sort")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Vertex sort")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VERTEX_SORT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VIEW_COORDINATE_KEY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("View coordinate key (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("View Key")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("View Key")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VIEW_COORDINATE_KEY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.QUERY_CLAUSES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.QUERY_CLAUSES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.WITHDRAWN_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Withdrawn state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Withdrawn")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are withdrawn.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.WITHDRAWN_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(STATUS_VALUE)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BOOLEAN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Boolean")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BOOLEAN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BYTE_ARRAY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Byte array (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Byte array")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BYTE_ARRAY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description list for concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Description list for concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("List of description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DOUBLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Double (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Double")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DOUBLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.FLOAT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Float")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.FLOAT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logic graph for semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Logic graph")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SEMANTIC_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LONG))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Long (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Long")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LONG.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.NID))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("NID (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Native Identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.NID.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic list for chronicle (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Semantic list for chronicle")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Semantic list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(USERS_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Users module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("User module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Module - user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(USERS_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(MODULE))
                .attach(new StatedAxiom()
                        .isA(MODULE)).attach(new TinkarBaseModel());

        //converting above concept "ROOT_VERTEX" to composer api format.
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.ROOT_VERTEX))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Integrated Knowledge Management (SOLOR)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach(new USDialect().acceptability(TinkarTerm.PREFERRED))
                .attach((new Synonym()
                        .text("Tinkar root concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect().acceptability(TinkarTerm.PREFERRED))
                .attach((new Definition()
                        .text("Terminologies that are represented in a harmonized manner")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect().acceptability(TinkarTerm.PREFERRED))
                .attach((new Identifier()
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER))
                        .identifier(TinkarTerm.ROOT_VERTEX.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(MODEL_CONCEPT, MEANING, OBJECT, ROLE, USER, ANNOTATION_TYPE, TinkarTerm.CREATIVE_COMMONS_BY_LICENSE, TinkarTerm.HEALTH_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX)).attach(new TinkarBaseModel());

        //Necessary Terms Filtered out in generation routine - START
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(QUERY_CLAUSES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Query clauses (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Query clauses")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A distinct component/query that serves a specific purpose")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(QUERY_CLAUSES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.BOOLEAN_REFERENCE, TinkarTerm.VIEW_COORDINATE_KEY)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FIELD_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Field substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Field substitution")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Replacing a placeholder variable in a field with a specific value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FIELD_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.BOOLEAN_SUBSTITUTION, TinkarTerm.CONCEPT_SUBSTITUTION, TinkarTerm.FLOAT_SUBSTITUTION, TinkarTerm.INSTANT_SUBSTITUTION)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TAXONOMY_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Taxonomy operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Taxonomy operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("An operator or set of operations applied within the context of a taxonomy")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TAXONOMY_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.LOGICALLY_EQUIVALENT_TO)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IMMUTABLECOORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("ImmutableCoordinate Properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("ImmutableCoordinate properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A set of values or data representing a point in space that one established cannot be changed?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IMMUTABLECOORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE, TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE, TinkarTerm.MODULES_FOR_STAMP_COORDINATE, TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE, TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE, TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE, TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE, TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE, TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE, TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE, TinkarTerm.ROOT_FOR_LOGIC_COORDINATE, TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE, TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE, TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE, TinkarTerm.PATH_FOR_PATH_COORDINATE, TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH, TinkarTerm.VERTEX_SORT, TinkarTerm.VERTEX_STATE_SET, STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE, INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE, CLASSIFIER_FOR_LOGIC_COORDINATE, TinkarTerm.POSITION_ON_PATH)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PURPOSE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Purpose")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Purpose")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The reason for which a Tinkar value in a pattern was created or for which it exist.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PURPOSE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ACTION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Action properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Action properties")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Attributes of an action object")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ACTION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.CONCEPT_CONSTRAINTS, TinkarTerm.CONCEPT_TO_FIND, TinkarTerm.ROLE_TYPE_TO_ADD, TinkarTerm.CONDITIONAL_TRIGGERS)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LITERAL_VALUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Literal value (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Literal value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Fixed Value/Constant?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LITERAL_VALUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.BOOLEAN_LITERAL, TinkarTerm.FLOAT_LITERAL, TinkarTerm.INSTANT_LITERAL)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIALECT_ASSEMBLAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dialect")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Specifies the dialect of the language.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIALECT_ASSEMBLAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.CZECH_DIALECT, ENGLISH_DIALECT_ASSEMBLAGE, TinkarTerm.FRENCH_DIALECT, TinkarTerm.IRISH_DIALECT, TinkarTerm.KOREAN_DIALECT, TinkarTerm.POLISH_DIALECT, TinkarTerm.RUSSIAN_DIALECT)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT)).attach(new TinkarBaseModel());


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DYNAMIC_COLUMN_DATA_TYPES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dynamic column data types (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Dynamic column data types")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Data storage system where the structure of the data can be altered or extended dynamically?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DYNAMIC_COLUMN_DATA_TYPES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ARRAY, TinkarTerm.BOOLEAN, TinkarTerm.BYTE_ARRAY, TinkarTerm.DOUBLE, TinkarTerm.FLOAT, TinkarTerm.LONG, TinkarTerm.SIGNED_INTEGER, TinkarTerm.STRING, TinkarTerm.UUID_DATA_TYPE)
                        .parents(MEANING))
                .attach(new StatedAxiom()
                        .isA(MEANING)).attach(new TinkarBaseModel());

        //Necessary Terms Filtered out in generation routine - END

        // TODO: Get coordinates to work via Komet's KometTerm
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(POSITION_ON_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(POSITION_ON_PATH.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text(POSITION_ON_PATH.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(POSITION_ON_PATH.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Stated assemblage")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred assemblage")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Concepts to classify")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Classifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(CLASSIFIER_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Dialect order")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("SOLOR overlay module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(SOLOR_OVERLAY_MODULE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("SOLOR module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(SOLOR_MODULE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Transitive Feature")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(TRANSITIVE_PROPERTY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Reflexive Feature")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(REFLEXIVE_PROPERTY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Laterality")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(LATERALITY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Has Active Ingredient")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(HAS_ACTIVE_INGREDIENT.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Has Dose Form")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(HAS_DOSE_FORM.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Unmodeled role concept")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(UNMODELED_ROLE_CONCEPT.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Dynamic referenced component restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(DYNAMIC_REFERENCED_COMPONENT_RESTRICTION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Existential restriction")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Existential restrictions describe objects that participate in at least one relationship along a specified property to objects of a specified class.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Intrinsic role")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(INTRINSIC_ROLE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Property pattern implication")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(PROPERTY_PATTERN_IMPLICATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("SnoRocket classifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(SNOROCKET_CLASSIFIER.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Property set")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(PROPERTY_SET.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Feature")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(FEATURE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Example UCUM Units")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The Unified Code for Units of Measure (UCUM) is a code system intended to " +
                                "include all units of measures being contemporarily used in international science, " +
                                "engineering, and business. (www.unitsofmeasure.org) This field contains example units " +
                                "of measures for this term expressed as UCUM units.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The relationships/axioms of a concept that have been inferred")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Identifier Value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The literal string value identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Maximum Value Operator; Maximum Domain Operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The Value Operator assigned to the Maximum Value in a Range")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Minimum Value Operator; Minimum Domain Operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The Value Operator assigned to the Minimum Value in a Range")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Value Range")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The range of values specific to a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Maximum Value; Max Value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The highest possible value for a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Minimum Value; Min Value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The lowest possible value for a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Stated Definition")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("Relationships/Axioms of a concept that have been explicitly stated and defined")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Value Constraint")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("A component has specific value requirements that needs to be met")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Value Constraint Source")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text("The source organization of that specifies the constraint")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
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
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
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
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
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
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PROPERTY_SEQUENCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES)).attach(new TinkarBaseModel());

        //Create Description Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.DESCRIPTION_PATTERN)
                        .meaning(TinkarTerm.DESCRIPTION_SEMANTIC)
                        .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                        .fieldDefinition(
                                TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION,
                                LANGUAGE,
                                TinkarTerm.COMPONENT_FIELD)
                        .fieldDefinition(
                                TinkarTerm.TEXT_FOR_DESCRIPTION,
                                TinkarTerm.DESCRIPTION,
                                TinkarTerm.STRING)
                        .fieldDefinition(
                                DESCRIPTION_CASE_SIGNIFICANCE,
                                DESCRIPTION_CASE_SIGNIFICANCE,
                                TinkarTerm.COMPONENT_FIELD)
                        .fieldDefinition(
                                DESCRIPTION_TYPE,
                                DESCRIPTION_TYPE,
                                TinkarTerm.COMPONENT_FIELD))
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

        //Create Stated Navigation Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.STATED_NAVIGATION_PATTERN)
                        .meaning(TinkarTerm.IS_A)
                        .purpose(TinkarTerm.IS_A)
                        .fieldDefinition(
                                TinkarTerm.RELATIONSHIP_DESTINATION,
                                TinkarTerm.IS_A,
                                TinkarTerm.COMPONENT_ID_SET_FIELD)
                        .fieldDefinition(
                                TinkarTerm.RELATIONSHIP_ORIGIN,
                                TinkarTerm.IS_A,
                                TinkarTerm.COMPONENT_ID_SET_FIELD))
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

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Stated navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(STATED_NAVIGATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());


        //Create Inferred Navigation Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.INFERRED_NAVIGATION_PATTERN)
                        .meaning(TinkarTerm.IS_A)
                        .purpose(TinkarTerm.IS_A)
                        .fieldDefinition(
                                TinkarTerm.RELATIONSHIP_DESTINATION,
                                TinkarTerm.IS_A,
                                TinkarTerm.COMPONENT_ID_SET_FIELD)
                        .fieldDefinition(
                                TinkarTerm.RELATIONSHIP_ORIGIN,
                                TinkarTerm.IS_A,
                                TinkarTerm.COMPONENT_ID_SET_FIELD))
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

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INFERRED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inferred navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred navigation")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED)))
                .attach((Definition definition) -> definition
                        .text(INFERRED_NAVIGATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(new USDialect().acceptability(PREFERRED)))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(DESCRIPTION_TYPE)).attach(new TinkarBaseModel());


        //Create Identifier Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.IDENTIFIER_PATTERN)
                        .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                        .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                        .fieldDefinition(
                                IDENTIFIER_SOURCE,
                                IDENTIFIER_SOURCE,
                                TinkarTerm.COMPONENT_FIELD)
                        .fieldDefinition(
                                IDENTIFIER_VALUE,
                                IDENTIFIER_VALUE,
                                TinkarTerm.STRING))
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

        //Create US Dialect Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.US_DIALECT_PATTERN)
                        .meaning(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                        .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                        .fieldDefinition(
                                TinkarTerm.US_ENGLISH_DIALECT,
                                DESCRIPTION_ACCEPTABILITY,
                                TinkarTerm.COMPONENT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("US Dialect Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("US Dialect Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        //Create GB Dialect Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.GB_DIALECT_PATTERN)
                        .meaning(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                        .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                        .fieldDefinition(
                                TinkarTerm.GB_ENGLISH_DIALECT,
                                DESCRIPTION_ACCEPTABILITY,
                                TinkarTerm.COMPONENT_FIELD))
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

        //Create Axiom Syntax Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.OWL_AXIOM_SYNTAX_PATTERN)
                        .meaning(TinkarTerm.AXIOM_SYNTAX)
                        .purpose(TinkarTerm.EXPRESS_AXIOM_SYNTAX)
                        .fieldDefinition(
                                AXIOM_SYNTAX,
                                TinkarTerm.EXPRESS_AXIOM_SYNTAX,
                                TinkarTerm.STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(OWL_AXIOM_SYNTAX_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        //Create Stated Definition Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN)
                        .meaning(TinkarTerm.STATED_DEFINITION)
                        .purpose(TinkarTerm.LOGICAL_DEFINITION)
                        .fieldDefinition(
                                TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS,
                                LOGICAL_DEFINITION,
                                TinkarTerm.DITREE_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Stated Axioms Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Stated definition pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        //Create Inferred Definition Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.EL_PLUS_PLUS_INFERRED_AXIOMS_PATTERN)
                        .meaning(TinkarTerm.INFERRED_DEFINITION)
                        .purpose(TinkarTerm.LOGICAL_DEFINITION)
                        .fieldDefinition(
                                TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS,
                                LOGICAL_DEFINITION,
                                TinkarTerm.DITREE_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("EL++ Inferred Axioms Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Inferred definition pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        //Create Path Membership Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.PATHS_PATTERN)
                        .meaning(TinkarTerm.PATH)
                        .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(PATHS_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.SOLOR_CONCEPT_ASSEMBLAGE)
                        .meaning(TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE)
                        .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(SOLOR_CONCEPT_ASSEMBLAGE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        //Create STAMP Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.STAMP_PATTERN)
                        .meaning(TinkarTerm.VERSION_PROPERTIES)
                        .purpose(TinkarTerm.VERSION_PROPERTIES)
                        .fieldDefinition(
                                STATUS_VALUE,
                                TinkarTerm.STATUS_FOR_VERSION,
                                TinkarTerm.COMPONENT_FIELD)
                        .fieldDefinition(
                                TinkarTerm.TIME_FOR_VERSION,
                                TinkarTerm.TIME_FOR_VERSION,
                                TinkarTerm.LONG)
                        .fieldDefinition(
                                TinkarTerm.AUTHOR_FOR_VERSION,
                                TinkarTerm.AUTHOR_FOR_VERSION,
                                TinkarTerm.COMPONENT_FIELD)
                        .fieldDefinition(
                                TinkarTerm.MODULE_FOR_VERSION,
                                TinkarTerm.MODULE_FOR_VERSION,
                                TinkarTerm.COMPONENT_FIELD)
                        .fieldDefinition(
                                TinkarTerm.PATH_FOR_VERSION,
                                TinkarTerm.PATH_FOR_VERSION,
                                TinkarTerm.COMPONENT_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(STAMP_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.PATH_ORIGINS_PATTERN)
                        .meaning(TinkarTerm.PATH_ORIGINS)
                        .purpose(TinkarTerm.PATH_ORIGINS)
                        .fieldDefinition(
                                TinkarTerm.PATH_CONCEPT,
                                TinkarTerm.PATH_CONCEPT,
                                TinkarTerm.COMPONENT_FIELD)
                        .fieldDefinition(
                                TinkarTerm.PATH_ORIGINS,
                                TinkarTerm.PATH_ORIGINS,
                                TinkarTerm.INSTANT_LITERAL))
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

        //Create Comment Pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.COMMENT_PATTERN)
                .meaning(TinkarTerm.COMMENT)
                .purpose(TinkarTerm.COMMENT)
                .fieldDefinition(
                        TinkarTerm.COMMENT,
                        TinkarTerm.COMMENT,
                        TinkarTerm.STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                .text(COMMENT_PATTERN.description())
                .language(ENGLISH_LANGUAGE)
                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach((Synonym synonym) -> synonym
                        .text("Comment Pattern")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        //Create Tinkar Core base model pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.TINKAR_BASE_MODEL_COMPONENT_PATTERN)
                        .meaning(TinkarTerm.STARTER_DATA_AUTHORING)
                        .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(TINKAR_BASE_MODEL_COMPONENT_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new TinkarBaseModel());

        //Create Komet base model component pattern
        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.KOMET_BASE_MODEL_COMPONENT_PATTERN)
                        .meaning(TinkarTerm.STARTER_DATA_AUTHORING)
                        .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(KOMET_BASE_MODEL_COMPONENT_PATTERN.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE))
                .attach(new KometBaseModel());

        session.compose((PatternAssembler patternAssembler) -> patternAssembler.pattern(TinkarTerm.VALUE_CONSTRAINT_PATTERN)
                        .meaning(TinkarTerm.VALUE_CONSTRAINT)
                        .purpose(TinkarTerm.VALUE_CONSTRAINT)
                        .fieldDefinition(
                                VALUE_CONSTRAINT_SOURCE,
                                VALUE_CONSTRAINT_SOURCE,
                                TinkarTerm.CONCEPT_FIELD)
                        .fieldDefinition(
                                MINIMUM_VALUE_OPERATOR,
                                CONCRETE_DOMAIN_OPERATOR,
                                TinkarTerm.CONCEPT_FIELD)
                        .fieldDefinition(
                                REFERENCE_RANGE_MINIMUM,
                                REFERENCE_RANGE,
                                TinkarTerm.FLOAT_FIELD)
                        .fieldDefinition(
                                MAXIMUM_VALUE_OPERATOR,
                                CONCRETE_DOMAIN_OPERATOR,
                                TinkarTerm.COMPONENT_FIELD)
                        .fieldDefinition(
                                REFERENCE_RANGE_MAXIMUM,
                                REFERENCE_RANGE,
                                TinkarTerm.FLOAT_FIELD)
                        .fieldDefinition(
                                EXAMPLE_UCUM_UNITS,
                                EXAMPLE_UCUM_UNITS,
                                TinkarTerm.STRING))
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

        addPathMembershipSemantics(session);
        addPathOriginSemantics(session);

        composer.commitSession(session);
    }

    private static void addPathMembershipSemantics(Session session) {
        addPathMembershipSemantic(session, DEVELOPMENT_PATH);
        addPathMembershipSemantic(session, SANDBOX_PATH);
        addPathMembershipSemantic(session, PRIMORDIAL_PATH);
        addPathMembershipSemantic(session, MASTER_PATH);
    }

    private static void addPathMembershipSemantic(Session session, EntityProxy.Concept concept) {
        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(TinkarTerm.PATHS_PATTERN)
                .reference(concept)
                .fieldValues(MutableList::newEmpty));
    }

    private static void addPathOriginSemantics(Session session) {
        addPathOriginSemantic(session, DEVELOPMENT_PATH, SANDBOX_PATH);
        addPathOriginSemantic(session, MASTER_PATH, DEVELOPMENT_PATH);
        addPathOriginSemantic(session, SANDBOX_PATH, PRIMORDIAL_PATH);
    }

    private static void addPathOriginSemantic(Session session, EntityProxy.Concept concept, EntityProxy.Concept originPath) {
        session.compose((SemanticAssembler semanticAssembler) -> semanticAssembler
                .pattern(TinkarTerm.PATH_ORIGINS_PATTERN)
                .reference(concept)
                .fieldValues(objects -> objects.addAll(Lists.mutable.of(originPath, DateTimeUtil.epochMsToInstant(Long.MAX_VALUE)))));

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
            LOG.error(e.getMessage());
            e.printStackTrace();
        }

        List<Integer> patternNids = new ArrayList<>();
        PrimitiveData.get().forEachPatternNid(patternNids::add);
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

        System.out.println("break");
    }
}
