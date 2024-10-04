package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.id.PublicIds;
import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.io.FileUtil;
import dev.ikm.tinkar.composer.Composer;
import dev.ikm.tinkar.composer.Session;
import dev.ikm.tinkar.composer.assembler.ConceptAssembler;
import dev.ikm.tinkar.composer.template.Definition;
import dev.ikm.tinkar.composer.template.FullyQualifiedName;
import dev.ikm.tinkar.composer.template.Identifier;
import dev.ikm.tinkar.composer.template.StatedAxiom;
import dev.ikm.tinkar.composer.template.StatedNavigation;
import dev.ikm.tinkar.composer.template.Synonym;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;

import static dev.ikm.tinkar.terms.TinkarTerm.ACTION_PROPERTIES;
import static dev.ikm.tinkar.terms.TinkarTerm.CASE_INSENSITIVE_EVALUATION;
import static dev.ikm.tinkar.terms.TinkarTerm.CHRONICLE_PROPERTIES;
import static dev.ikm.tinkar.terms.TinkarTerm.CLASSIFIER_FOR_LOGIC_COORDINATE;
import static dev.ikm.tinkar.terms.TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE;
import static dev.ikm.tinkar.terms.TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE;
import static dev.ikm.tinkar.terms.TinkarTerm.DIALECT_ASSEMBLAGE;
import static dev.ikm.tinkar.terms.TinkarTerm.DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE;
import static dev.ikm.tinkar.terms.TinkarTerm.DISPLAY_FIELDS;
import static dev.ikm.tinkar.terms.TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES;
import static dev.ikm.tinkar.terms.TinkarTerm.DYNAMIC_REFERENCED_COMPONENT_RESTRICTION;
import static dev.ikm.tinkar.terms.TinkarTerm.ENGLISH_DIALECT_ASSEMBLAGE;
import static dev.ikm.tinkar.terms.TinkarTerm.ENGLISH_LANGUAGE;
import static dev.ikm.tinkar.terms.TinkarTerm.EXAMPLE_UCUM_UNITS;
import static dev.ikm.tinkar.terms.TinkarTerm.EXISTENTIAL_RESTRICTION;
import static dev.ikm.tinkar.terms.TinkarTerm.FEATURE;
import static dev.ikm.tinkar.terms.TinkarTerm.FIELD_SUBSTITUTION;
import static dev.ikm.tinkar.terms.TinkarTerm.HAS_ACTIVE_INGREDIENT;
import static dev.ikm.tinkar.terms.TinkarTerm.HAS_DOSE_FORM;
import static dev.ikm.tinkar.terms.TinkarTerm.IDENTIFIER_SOURCE;
import static dev.ikm.tinkar.terms.TinkarTerm.IDENTIFIER_VALUE;
import static dev.ikm.tinkar.terms.TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES;
import static dev.ikm.tinkar.terms.TinkarTerm.INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE;
import static dev.ikm.tinkar.terms.TinkarTerm.INFERRED_DEFINITION;
import static dev.ikm.tinkar.terms.TinkarTerm.INFERRED_NAVIGATION;
import static dev.ikm.tinkar.terms.TinkarTerm.INTRINSIC_ROLE;
import static dev.ikm.tinkar.terms.TinkarTerm.LANGUAGE;
import static dev.ikm.tinkar.terms.TinkarTerm.LATERALITY;
import static dev.ikm.tinkar.terms.TinkarTerm.LITERAL_VALUE;
import static dev.ikm.tinkar.terms.TinkarTerm.MAXIMUM_VALUE_OPERATOR;
import static dev.ikm.tinkar.terms.TinkarTerm.MINIMUM_VALUE_OPERATOR;
import static dev.ikm.tinkar.terms.TinkarTerm.OBJECT;
import static dev.ikm.tinkar.terms.TinkarTerm.OBJECT_PROPERTIES;
import static dev.ikm.tinkar.terms.TinkarTerm.PHENOMENON;
import static dev.ikm.tinkar.terms.TinkarTerm.POSITION_ON_PATH;
import static dev.ikm.tinkar.terms.TinkarTerm.PREFERRED;
import static dev.ikm.tinkar.terms.TinkarTerm.PROPERTY_PATTERN_IMPLICATION;
import static dev.ikm.tinkar.terms.TinkarTerm.PROPERTY_SET;
import static dev.ikm.tinkar.terms.TinkarTerm.PURPOSE;
import static dev.ikm.tinkar.terms.TinkarTerm.QUERY_CLAUSES;
import static dev.ikm.tinkar.terms.TinkarTerm.REFERENCE_RANGE;
import static dev.ikm.tinkar.terms.TinkarTerm.REFERENCE_RANGE_MAXIMUM;
import static dev.ikm.tinkar.terms.TinkarTerm.REFERENCE_RANGE_MINIMUM;
import static dev.ikm.tinkar.terms.TinkarTerm.REFLEXIVE_PROPERTY;
import static dev.ikm.tinkar.terms.TinkarTerm.ROLE_OPERATOR;
import static dev.ikm.tinkar.terms.TinkarTerm.ROOT_VERTEX;
import static dev.ikm.tinkar.terms.TinkarTerm.SNOROCKET_CLASSIFIER;
import static dev.ikm.tinkar.terms.TinkarTerm.SOLOR_MODULE;
import static dev.ikm.tinkar.terms.TinkarTerm.SOLOR_OVERLAY_MODULE;
import static dev.ikm.tinkar.terms.TinkarTerm.STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE;
import static dev.ikm.tinkar.terms.TinkarTerm.STATED_DEFINITION;
import static dev.ikm.tinkar.terms.TinkarTerm.STATED_NAVIGATION;
import static dev.ikm.tinkar.terms.TinkarTerm.TAXONOMY_OPERATOR;
import static dev.ikm.tinkar.terms.TinkarTerm.TEXT_COMPARISON_MEASURE_SEMANTIC;
import static dev.ikm.tinkar.terms.TinkarTerm.TINKAR_MODEL_CONCEPT;
import static dev.ikm.tinkar.terms.TinkarTerm.TRANSITIVE_PROPERTY;
import static dev.ikm.tinkar.terms.TinkarTerm.TREE_AMALGAM_PROPERTIES;
import static dev.ikm.tinkar.terms.TinkarTerm.UNMODELED_ROLE_CONCEPT;
import static dev.ikm.tinkar.terms.TinkarTerm.USERS_MODULE;
import static dev.ikm.tinkar.terms.TinkarTerm.US_ENGLISH_DIALECT;
import static dev.ikm.tinkar.terms.TinkarTerm.US_NURSING_DIALECT;
import static dev.ikm.tinkar.terms.TinkarTerm.VALUE_CONSTRAINT;
import static dev.ikm.tinkar.terms.TinkarTerm.VALUE_CONSTRAINT_SOURCE;
import static dev.ikm.tinkar.terms.TinkarTerm.VERSION_PROPERTIES;


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
                        TinkarTerm.USER,
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
                TinkarTerm.USER,
                TinkarTerm.PRIMORDIAL_MODULE,
                TinkarTerm.PRIMORDIAL_PATH);

        starterData.concept(ENGLISH_DIALECT_ASSEMBLAGE)
                .fullyQualifiedName("English Dialect", TinkarTerm.PREFERRED)
                .synonym("English dialect", TinkarTerm.PREFERRED)
                .definition("Specifies the dialect of the English language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, ENGLISH_DIALECT_ASSEMBLAGE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.GB_ENGLISH_DIALECT, TinkarTerm.US_ENGLISH_DIALECT), List.of(DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(DIALECT_ASSEMBLAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TEXT_COMPARISON_MEASURE_SEMANTIC)
                .fullyQualifiedName("Text comparison measure semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Text comparison", TinkarTerm.PREFERRED)
                .definition("Text comparison with a focus on semantic meaning involves evaluating the similarity or relatedness between pieces of text based on their underlying meaning rather than just their surface structure.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TEXT_COMPARISON_MEASURE_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CASE_INSENSITIVE_EVALUATION, TinkarTerm.CASE_SENSITIVE_EVALUATION), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.STARTER_DATA_AUTHORING)
                .fullyQualifiedName(TinkarTerm.STARTER_DATA_AUTHORING.description(), TinkarTerm.PREFERRED)
                .synonym("Metadata Authoring", TinkarTerm.ACCEPTABLE)
                .definition("Define necessary minimum viable concepts to use Tinkar Data", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STARTER_DATA_AUTHORING.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.AXIOM_SYNTAX)
                .fullyQualifiedName(TinkarTerm.AXIOM_SYNTAX.description(), TinkarTerm.PREFERRED)
                .synonym("Axiom Syntax", TinkarTerm.ACCEPTABLE)
                .definition("Syntax defining description logic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AXIOM_SYNTAX.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EXPRESS_AXIOM_SYNTAX)
                .fullyQualifiedName(TinkarTerm.EXPRESS_AXIOM_SYNTAX.description(), TinkarTerm.PREFERRED)
                .synonym("Express Axiom", TinkarTerm.ACCEPTABLE)
                .definition("Expressing description logic through syntax", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXPRESS_AXIOM_SYNTAX.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.AXIOM_SYNTAX))
                .statedDefinition(List.of(TinkarTerm.AXIOM_SYNTAX))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ACCEPTABLE)
                .fullyQualifiedName("Acceptable (foundation metadata concept)", TinkarTerm.PREFERRED)
                .synonym("Acceptable", TinkarTerm.PREFERRED)
                .definition("Specifies that a description is acceptable, but not preferred within a language or dialect.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ACCEPTABLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ACTIVE_STATE)
                .fullyQualifiedName("Active state", TinkarTerm.PREFERRED)
                .synonym("Active", TinkarTerm.PREFERRED)
                .definition("Concept used to represent a status for components that are active.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ACTIVE_STATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.STATUS_VALUE))
                .statedDefinition(List.of(TinkarTerm.STATUS_VALUE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Allowed states for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Allowed states", TinkarTerm.PREFERRED)
                .definition("Predefined list of values for STAMP coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.AND)
                .fullyQualifiedName("And (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("And", TinkarTerm.PREFERRED)
                .definition("An operator that typically is employed to combine two conditions", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AND.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ANNOTATION_TYPE)
                .fullyQualifiedName("Annotation type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Annotation type", TinkarTerm.PREFERRED)
                .definition("Metadata about program elements, and annotation types define the structure of these annotations", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANNOTATION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.COMMENT, TinkarTerm.KOMET_ISSUE), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ANONYMOUS_CONCEPT)
                .fullyQualifiedName("Anonymous concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Anonymous concept", TinkarTerm.PREFERRED)
                .definition("Concepts or entities that do not have a specific, named identity, (defined on-the-fly without a dedicated name)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANONYMOUS_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCEPT_TYPE))
                .statedDefinition(List.of(TinkarTerm.CONCEPT_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ANY_COMPONENT)
                .fullyQualifiedName("Any component (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Any component", TinkarTerm.PREFERRED)
                .definition("A general-purpose container to represent any component with generic data structure. Modifiable based on the specific requirements and characteristics of the components.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANY_COMPONENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(OBJECT))
                .statedDefinition(List.of(OBJECT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ARRAY)
                .fullyQualifiedName("Array (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Array", TinkarTerm.PREFERRED)
                .definition("Linear data structure", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ARRAY.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(DYNAMIC_COLUMN_DATA_TYPES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ARRAY_FIELD)
                .fullyQualifiedName("Array field (Solor)", TinkarTerm.PREFERRED)
                .synonym("Array field", TinkarTerm.PREFERRED)
                .definition("A lexical set of semantically related elements/items", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ARRAY_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE)
                .fullyQualifiedName("Author for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Author", TinkarTerm.PREFERRED)
                .definition("Individual or entity who made a particular edit or revision in a document (authoring a specific location or point in the codebase where an edit was made)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.AUTHOR_FOR_VERSION)
                .fullyQualifiedName("Author for version (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Author", TinkarTerm.PREFERRED)
                .definition("Individual or entity who made a specific set of changes or modifications to a codebase/terminology resulting in the creation of a new version or revision", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHOR_FOR_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(VERSION_PROPERTIES))
                .statedDefinition(List.of(VERSION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Author for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Authors", TinkarTerm.PREFERRED)
                .definition("In individual or an entity responsible for defining or updating the values associated with the STAMP coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.AXIOM_FOCUS)
                .fullyQualifiedName("Axiom focus (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Axiom focus", TinkarTerm.PREFERRED)
                .definition("A statement or proposition that is assumed to be true without requiring proof, it serves as a foundation principles on which a system or theory is built. Focus refers to the central point of attention or concentration on a specific concept/axioms", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AXIOM_FOCUS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .statedDefinition(List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.AXIOM_ORIGIN)
                .fullyQualifiedName("Axiom origin (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Axiom origin", TinkarTerm.PREFERRED)
                .definition("The parent concept for the axiom?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AXIOM_ORIGIN.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.STATED_PREMISE_TYPE, TinkarTerm.INFERRED_PREMISE_TYPE), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_FIELD)
                .fullyQualifiedName("Boolean field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean field", TinkarTerm.PREFERRED)
                .definition("True (1) or false (0)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_LITERAL)
                .fullyQualifiedName("Boolean literal (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean literal", TinkarTerm.PREFERRED)
                .definition("TRUE, FALSE, UNKNOWN", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_LITERAL.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LITERAL_VALUE))
                .statedDefinition(List.of(LITERAL_VALUE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_REFERENCE)
                .fullyQualifiedName("Boolean reference (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean reference", TinkarTerm.PREFERRED)
                .definition("Reference(a pointer) to a Boolean object", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_REFERENCE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(QUERY_CLAUSES))
                .statedDefinition(List.of(QUERY_CLAUSES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_SUBSTITUTION)
                .fullyQualifiedName("Boolean substitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean substitution", TinkarTerm.PREFERRED)
                .definition("The process of replacing or substituting boolean values or expression in a logical context", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(FIELD_SUBSTITUTION))
                .statedDefinition(List.of(FIELD_SUBSTITUTION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.BYTE_ARRAY_FIELD)
                .fullyQualifiedName("Byte array field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Byte array field", TinkarTerm.PREFERRED)
                .definition("An array of bytes", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BYTE_ARRAY_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CANCELED_STATE)
                .fullyQualifiedName("Canceled state", TinkarTerm.PREFERRED)
                .synonym("Canceled", TinkarTerm.PREFERRED)
                .definition("Concept used to represent a status for components that are canceled", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CANCELED_STATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.STATUS_VALUE))
                .statedDefinition(List.of(TinkarTerm.STATUS_VALUE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CASE_INSENSITIVE_EVALUATION)
                .fullyQualifiedName("Case insensitive evaluation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Ignore case", TinkarTerm.PREFERRED)
                .definition("Evaluates values regardless of the case", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_INSENSITIVE_EVALUATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .statedDefinition(List.of(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CASE_SENSITIVE_EVALUATION)
                .fullyQualifiedName("Case sensitive evaluation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Compare case", TinkarTerm.PREFERRED)
                .definition("Evaluated based on the case", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_SENSITIVE_EVALUATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .statedDefinition(List.of(TEXT_COMPARISON_MEASURE_SEMANTIC))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION)
                .fullyQualifiedName("Case significance concept nid for description (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Case significance", TinkarTerm.PREFERRED)
                .definition("A field label which captures the case significance for a given concept description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CHINESE_LANGUAGE)
                .fullyQualifiedName("Chinese language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Chinese language", TinkarTerm.PREFERRED)
                .definition("Chinese language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CHINESE_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(CHRONICLE_PROPERTIES)
                .fullyQualifiedName("Chronicle properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Chronicle properties", TinkarTerm.PREFERRED)
                .definition("Attributes or characteristic associated with a historical record or an account of events (metadata, timestamps)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, CHRONICLE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE, TinkarTerm.VERSION_LIST_FOR_CHRONICLE, TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE, TinkarTerm.UUID_LIST_FOR_COMPONENT), List.of(OBJECT_PROPERTIES))
                .statedDefinition(List.of(OBJECT_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.COMMENT)
                .fullyQualifiedName("Comment (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Comment", TinkarTerm.PREFERRED)
                .definition("A filed label to capture free text information which may be necessary to add or change (concepts, relationships, semantics, etc)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMMENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ANNOTATION_TYPE))
                .statedDefinition(List.of(TinkarTerm.ANNOTATION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.COMPONENT_ID_LIST_FIELD)
                .fullyQualifiedName("Component Id list", TinkarTerm.PREFERRED)
                .synonym("Component Id list", TinkarTerm.PREFERRED)
                .definition("A display field that references an ordered list of Concept IDs.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_ID_LIST_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fullyQualifiedName("Component Id set field", TinkarTerm.PREFERRED)
                .synonym("Component Id set", TinkarTerm.PREFERRED)
                .definition("A display field that references an unordered list of Concept IDs.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_ID_SET_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.COMPONENT_FIELD)
                .fullyQualifiedName("Component field", TinkarTerm.PREFERRED)
                .synonym("Component field", TinkarTerm.PREFERRED)
                .definition("A display field type that references a concept ID.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.COMPONENT_FOR_SEMANTIC)
                .fullyQualifiedName("Component for semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component", TinkarTerm.PREFERRED)
                .definition("Component for semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_FOR_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.COMPONENT_SEMANTIC)
                .fullyQualifiedName("Component semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component Semantic", TinkarTerm.PREFERRED)
                .definition("Component semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.COMPONENT_TYPE_FOCUS)
                .fullyQualifiedName("Component type focus (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component type focus", TinkarTerm.PREFERRED)
                .definition("Focus type of component", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_TYPE_FOCUS.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.AXIOM_FOCUS, TinkarTerm.CONCEPT_FOCUS, TinkarTerm.DESCRIPTION_FOCUS), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_CONSTRAINTS)
                .fullyQualifiedName("Concept constraints(SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept constraints", TinkarTerm.PREFERRED)
                .definition("Defined filters for a given concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_CONSTRAINTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(ACTION_PROPERTIES))
                .statedDefinition(List.of(ACTION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE)
                .fullyQualifiedName("Concept details tree table (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept details tree table", TinkarTerm.PREFERRED)
                .definition("Tree table with concept details", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_DETAILS_TREE_TABLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_FIELD)
                .fullyQualifiedName("Concept field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym(" Concept field", TinkarTerm.PREFERRED)
                .definition("Field for the human readable description for the given concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_FOCUS)
                .fullyQualifiedName("Concept focus (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept focus", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_FOCUS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .statedDefinition(List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_REFERENCE)
                .fullyQualifiedName("Concept reference (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept reference", TinkarTerm.PREFERRED)
                .definition("A field to capture a reference to validate concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_REFERENCE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_SEMANTIC)
                .fullyQualifiedName("Concept semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept semantic", TinkarTerm.PREFERRED)
                .definition("Value to define a given semantic as a concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_SUBSTITUTION)
                .fullyQualifiedName("Concept substitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept substitution", TinkarTerm.PREFERRED)
                .definition("Substitution for concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(FIELD_SUBSTITUTION))
                .statedDefinition(List.of(FIELD_SUBSTITUTION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_TO_FIND)
                .fullyQualifiedName("Concept to find (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept to find", TinkarTerm.PREFERRED)
                .definition("Find concept (if searching on Komet shows us the results 'details and further information?)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_TO_FIND.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(ACTION_PROPERTIES))
                .statedDefinition(List.of(ACTION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_TYPE)
                .fullyQualifiedName("Concept type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept type", TinkarTerm.PREFERRED)
                .definition("A field that captures a defined concept label", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_TYPE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ANONYMOUS_CONCEPT, TinkarTerm.PATH_CONCEPT, TinkarTerm.SEMANTIC_FIELD_CONCEPTS), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCEPT_VERSION)
                .fullyQualifiedName("Concept version (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Version", TinkarTerm.PREFERRED)
                .definition("A filed that captures the version of the terminology that it came from", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONCRETE_DOMAIN_OPERATOR)
                .fullyQualifiedName("Concrete value operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concrete value operator", TinkarTerm.PREFERRED)
                .definition("A concept that specifies value operators", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCRETE_DOMAIN_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EQUAL_TO, TinkarTerm.GREATER_THAN, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, TinkarTerm.LESS_THAN, TinkarTerm.LESS_THAN_OR_EQUAL_TO, MAXIMUM_VALUE_OPERATOR, MINIMUM_VALUE_OPERATOR), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONDITIONAL_TRIGGERS)
                .fullyQualifiedName("Conditional triggers (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Conditional triggers", TinkarTerm.PREFERRED)
                .definition("Conditional triggers based on actions, reasoner", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONDITIONAL_TRIGGERS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(ACTION_PROPERTIES))
                .statedDefinition(List.of(ACTION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CONNECTIVE_OPERATOR)
                .fullyQualifiedName("Connective operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Connective operator", TinkarTerm.PREFERRED)
                .definition("A field that captures what the operator is (logical connective)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONNECTIVE_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.AND, TinkarTerm.DEFINITION_ROOT, TinkarTerm.DISJOINT_WITH, TinkarTerm.OR, TinkarTerm.IS_A, TinkarTerm.PART_OF, TinkarTerm.CONCEPT_REFERENCE), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CORELATION_EXPRESSION)
                .fullyQualifiedName("Correlation expression (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Correlation expression", TinkarTerm.PREFERRED)
                .definition("A value for Correlation properties", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CORELATION_EXPRESSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CORRELATION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.CORRELATION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CORELATION_REFERENCE_EXPRESSION)
                .fullyQualifiedName("Correlation reference expression (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Correlation reference expression", TinkarTerm.PREFERRED)
                .definition("A value for correlation", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CORELATION_REFERENCE_EXPRESSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CORRELATION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.CORRELATION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CORRELATION_PROPERTIES)
                .fullyQualifiedName("Correlation properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Correlation properties", TinkarTerm.PREFERRED)
                .definition("Characteristics or measures that describe the relationship between two or more variables", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CORRELATION_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CORELATION_EXPRESSION, TinkarTerm.CORELATION_REFERENCE_EXPRESSION), List.of(OBJECT_PROPERTIES))
                .statedDefinition(List.of(OBJECT_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE)
                .fullyQualifiedName("Creative Commons BY license (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Creative Commons BY license", TinkarTerm.PREFERRED)
                .definition("Creative Commons (CC) licenses are a set of public copyright licenses that enable the free distribution of an otherwise copyrighted work", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CREATIVE_COMMONS_BY_LICENSE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CZECH_DIALECT)
                .fullyQualifiedName("Czech dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Czech dialect", TinkarTerm.PREFERRED)
                .definition("Czech dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CZECH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(DIALECT_ASSEMBLAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.CZECH_LANGUAGE)
                .fullyQualifiedName("Czech language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Czech language", TinkarTerm.PREFERRED)
                .definition("Czech Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CZECH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DANISH_LANGUAGE)
                .fullyQualifiedName("Danish language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Danish language", TinkarTerm.PREFERRED)
                .definition("Danish Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DANISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE)
                .fullyQualifiedName("Default module for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Default module", TinkarTerm.PREFERRED)
                .definition("A value for coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DEFINITION_DESCRIPTION_TYPE)
                .fullyQualifiedName("Definition description type", TinkarTerm.PREFERRED)
                .synonym("Definition", TinkarTerm.PREFERRED)
                .definition("Semantic value describing the description type for the description pattern is a definition", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFINITION_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DEFINITION_ROOT)
                .fullyQualifiedName("Definition root (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Definition root", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFINITION_ROOT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION)
                .fullyQualifiedName("Description", TinkarTerm.PREFERRED)
                .synonym("Description", TinkarTerm.PREFERRED)
                .definition("Human readable text for a concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                .fullyQualifiedName("Description acceptability", TinkarTerm.PREFERRED)
                .synonym("Description acceptability", TinkarTerm.PREFERRED)
                .definition("Whether a given human readable text for a concept is permissible", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_ACCEPTABILITY.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ACCEPTABLE, TinkarTerm.PREFERRED), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CASE_SENSITIVE)
                .fullyQualifiedName("Description case sensitive", TinkarTerm.PREFERRED)
                .synonym("Case sensitive", TinkarTerm.PREFERRED)
                .definition("Assumes the description is dependent on capitalization", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CASE_SENSITIVE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE)
                .fullyQualifiedName("Description case significance", TinkarTerm.PREFERRED)
                .synonym("Description case significance", TinkarTerm.PREFERRED)
                .definition("Specifies how to handle the description text in terms of case sensitivity", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DESCRIPTION_CASE_SENSITIVE, TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CORE_TYPE)
                .fullyQualifiedName("Description core type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description core type", TinkarTerm.PREFERRED)
                .definition("Used to mark non-snomed descriptions as one of the core snomed types", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CORE_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_DIALECT_PAIR)
                .fullyQualifiedName("Description dialect pair (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description dialect pair", TinkarTerm.PREFERRED)
                .definition("Description dialect pair - linking together dialects with language descriptions", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_DIALECT_PAIR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR, TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR), List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_FOCUS)
                .fullyQualifiedName("Description focus (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description focus", TinkarTerm.PREFERRED)
                .definition("Description focus", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_FOCUS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .statedDefinition(List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR)
                .fullyQualifiedName("Description for dialect/description pair (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description for dialect/description pair", TinkarTerm.PREFERRED)
                .definition("Linking together dialects with language descriptions", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE)
                .fullyQualifiedName("Description initial character case sensitive (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Initial character case insensitive", TinkarTerm.PREFERRED)
                .definition("Value which designates initial character as sensitive for a given description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName("Description logic profile for logic coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logic profile", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE)
                .fullyQualifiedName("Description not case sensitive", TinkarTerm.PREFERRED)
                .synonym("Case insensitive", TinkarTerm.PREFERRED)
                .definition("Value which designate character as not sensitive for a given description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fullyQualifiedName("Description semantic", TinkarTerm.PREFERRED)
                .synonym("Description semantic", TinkarTerm.PREFERRED)
                .definition("Purpose and meaning for the description pattern and dialect patterns", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE)
                .fullyQualifiedName("Description type", TinkarTerm.PREFERRED)
                .synonym("Description type", TinkarTerm.PREFERRED)
                .definition("Specifying what type of description it is i.e. is it fully qualified or regular and etc.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DEFINITION_DESCRIPTION_TYPE, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION)
                .fullyQualifiedName("Description type for description (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description type", TinkarTerm.PREFERRED)
                .definition("Linking for each description -> what type it is", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName("Description type preference list for language coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Type order", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES)
                .fullyQualifiedName("Description version properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description version properties", TinkarTerm.PREFERRED)
                .definition("Combination of terms that might be used in a specific context or domain", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_VERSION_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION, TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION, TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION, TinkarTerm.DESCRIPTION_DIALECT_PAIR), List.of(VERSION_PROPERTIES))
                .statedDefinition(List.of(VERSION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE)
                .fullyQualifiedName("Description-logic profile (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description-logic profile", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_LOGIC_PROFILE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EL_PLUS_PLUS_PROFILE), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE)
                .fullyQualifiedName("Destination module for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Destination module", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DEVELOPMENT_MODULE)
                .fullyQualifiedName("Development module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Development module", TinkarTerm.PREFERRED)
                .definition("Predefines or standard module within a system or application that is specifically designed to support the development phase of a project", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEVELOPMENT_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODULE))
                .statedDefinition(List.of(TinkarTerm.MODULE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DEVELOPMENT_PATH)
                .fullyQualifiedName("Development path", TinkarTerm.PREFERRED)
                .synonym("Development path", TinkarTerm.PREFERRED)
                .definition("A path that specifies that the components are currently under development", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEVELOPMENT_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH))
                .statedDefinition(List.of(TinkarTerm.PATH))
                .pathMembership()
                .pathOrigin(TinkarTerm.SANDBOX_PATH)
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DIGRAPH_FIELD)
                .fullyQualifiedName("DiGraph field", TinkarTerm.PREFERRED)
                .synonym("Instant/ DiGraph", TinkarTerm.PREFERRED)
                .definition("A display field that references a di-graph whose edges are ordered pairs of vertices. Each edge can be followed from one vertex to another vertex.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIGRAPH_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DITREE_FIELD)
                .fullyQualifiedName("DiTree field", TinkarTerm.PREFERRED)
                .synonym("DiTree", TinkarTerm.PREFERRED)
                .definition("A display field that references a graph obtained from an undirected tree by replacing each undirected edge by two directed edges with opposite directions.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DITREE_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR)
                .fullyQualifiedName("Dialect for dialect/description pair (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Dialect for dialect/description pair", TinkarTerm.PREFERRED)
                .definition("Specific dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName("Digraph for logic coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Digraph", TinkarTerm.PREFERRED)
                .definition("A value which describes a immutable coordinate property", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DIRECTED_GRAPH)
                .fullyQualifiedName("Directed graph (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("NavigationCoordinate/Directed graph", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIRECTED_GRAPH.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EL_PLUS_PLUS_DIGRAPH), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DISJOINT_WITH)
                .fullyQualifiedName("Disjoint with (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Disjoint with", TinkarTerm.PREFERRED)
                .definition("An operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DISJOINT_WITH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(DISPLAY_FIELDS)
                .fullyQualifiedName("Display Fields", TinkarTerm.PREFERRED)
                .synonym("Display fields", TinkarTerm.PREFERRED)
                .definition("Captures the human readable terms", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, DISPLAY_FIELDS.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.COMPONENT_FIELD, TinkarTerm.COMPONENT_ID_LIST_FIELD, TinkarTerm.COMPONENT_ID_SET_FIELD, TinkarTerm.CONCEPT_FIELD, TinkarTerm.DIGRAPH_FIELD, TinkarTerm.DITREE_FIELD, TinkarTerm.FLOAT_FIELD, TinkarTerm.INTEGER_FIELD, TinkarTerm.SEMANTIC_FIELD_TYPE, TinkarTerm.STRING), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DOUBLE_FIELD)
                .fullyQualifiedName("Double field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Double field", TinkarTerm.PREFERRED)
                .definition("A data value (type of structure for data)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DOUBLE_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.DUTCH_LANGUAGE)
                .fullyQualifiedName("Dutch language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Dutch language", TinkarTerm.PREFERRED)
                .definition("Dutch language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DUTCH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EL_PROFILE_SET_OPERATOR)
                .fullyQualifiedName("El profile set operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL profile set operator", TinkarTerm.PREFERRED)
                .definition("EL profile set operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PROFILE_SET_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.NECESSARY_SET, TinkarTerm.SUFFICIENT_SET), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .tinkarBaseModelMembership()
                .build();


        starterData.concept(TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS)
                .fullyQualifiedName("EL++ terminological axioms", TinkarTerm.PREFERRED)
                .synonym("EL++ terminological axioms", TinkarTerm.PREFERRED)
                .definition("The set of relationships or axioms has defined by the EL++ Logic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION)
                .fullyQualifiedName("El++ Inferred Concept Definition (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL++ Inferred Concept Definition", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LOGICAL_DEFINITION))
                .statedDefinition(List.of(TinkarTerm.LOGICAL_DEFINITION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)
                .fullyQualifiedName("EL++ Inferred terminological axioms", TinkarTerm.PREFERRED)
                .synonym("EL++ Inferred terminological axioms", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.NECESSARY_SET, TinkarTerm.SUFFICIENT_SET, TinkarTerm.INCLUSION_SET, TinkarTerm.ROLE, TinkarTerm.ROLE_GROUP), List.of(TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .statedDefinition(List.of(TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION)
                .fullyQualifiedName("EL++ Stated Concept Definition (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL++ Stated Concept Definition", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LOGICAL_DEFINITION))
                .statedDefinition(List.of(TinkarTerm.LOGICAL_DEFINITION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)
                .fullyQualifiedName("EL++ Stated terminological axioms", TinkarTerm.PREFERRED)
                .synonym("EL++ Stated terminological axioms", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.NECESSARY_SET, TinkarTerm.SUFFICIENT_SET, TinkarTerm.INCLUSION_SET, TinkarTerm.ROLE, TinkarTerm.ROLE_GROUP), List.of(TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .statedDefinition(List.of(TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_DIGRAPH)
                .fullyQualifiedName("EL++ digraph (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL++ digraph", TinkarTerm.PREFERRED)
                .definition("The directed graph that results from classifying a set of EL++ axioms", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_DIGRAPH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DIRECTED_GRAPH))
                .statedDefinition(List.of(TinkarTerm.DIRECTED_GRAPH))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_PROFILE)
                .fullyQualifiedName("EL++ logic profile (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL ++ logic profile", TinkarTerm.PREFERRED)
                .definition("EL ++ profile", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_PROFILE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_LOGIC_PROFILE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_LOGIC_PROFILE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ENGLISH_LANGUAGE)
                .fullyQualifiedName("English Language", TinkarTerm.PREFERRED)
                .synonym("English language", TinkarTerm.PREFERRED)
                .definition("Value for description language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ENGLISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EQUAL_TO)
                .fullyQualifiedName("Equal to (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Equal to", TinkarTerm.PREFERRED)
                .definition("A concept indicating the operator \"=\"", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EQUAL_TO.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EXACT)
                .fullyQualifiedName("Exact (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Exact", TinkarTerm.PREFERRED)
                .definition("Source and target are semantic or exact lexical match", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXACT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.GROUPING))
                .statedDefinition(List.of(TinkarTerm.GROUPING))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EXTENDED_DESCRIPTION_TYPE)
                .fullyQualifiedName("Extended description type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Extended description type", TinkarTerm.PREFERRED)
                .definition("Used to store non-snomed description types when other terminologies are imported", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXTENDED_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE)
                .fullyQualifiedName("Extended relationship type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Extended relationship type", TinkarTerm.PREFERRED)
                .definition("Used to store non-snomed relationship types when other terminologies are imported- especially when a relationship is mapped onto a snomed relationship type (such as isa)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXTENDED_RELATIONSHIP_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.FLOAT_FIELD)
                .fullyQualifiedName("Float field", TinkarTerm.PREFERRED)
                .synonym("Float field", TinkarTerm.PREFERRED)
                .definition("Represents values as high-precision fractional values.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.FLOAT_LITERAL)
                .fullyQualifiedName("Float literal (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Float literal", TinkarTerm.PREFERRED)
                .definition("Numbers with decimal point or an exponential part", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_LITERAL.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LITERAL_VALUE))
                .statedDefinition(List.of(LITERAL_VALUE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.FLOAT_SUBSTITUTION)
                .fullyQualifiedName("Float substitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Float substitution", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(FIELD_SUBSTITUTION))
                .statedDefinition(List.of(FIELD_SUBSTITUTION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.FRENCH_DIALECT)
                .fullyQualifiedName("French dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("French dialect", TinkarTerm.PREFERRED)
                .definition("French dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FRENCH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(DIALECT_ASSEMBLAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.FRENCH_LANGUAGE)
                .fullyQualifiedName("French Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("French language", TinkarTerm.PREFERRED)
                .definition("French Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FRENCH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE)
                .fullyQualifiedName("Fully qualified name description type", TinkarTerm.PREFERRED)
                .synonym("Fully qualified name", TinkarTerm.PREFERRED)
                .definition("Fully qualified name is a description that uniquely identifies and differentiates it from other concepts with similar descriptions", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.GB_ENGLISH_DIALECT)
                .fullyQualifiedName("Great Britain English dialect", TinkarTerm.PREFERRED)
                .synonym("GB English dialect / GB English", TinkarTerm.PREFERRED)
                .definition("Great Britain: English Language reference set", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GB_ENGLISH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(ENGLISH_DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(ENGLISH_DIALECT_ASSEMBLAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.GERMAN_LANGUAGE)
                .fullyQualifiedName("German Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("German language", TinkarTerm.PREFERRED)
                .definition("German Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GERMAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.GREATER_THAN)
                .fullyQualifiedName("Greater than (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Greater than", TinkarTerm.PREFERRED)
                .definition("A concept indicating the operator \">\"", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GREATER_THAN.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.GREATER_THAN_OR_EQUAL_TO)
                .fullyQualifiedName("Greater than or equal to (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Greater than or equal to", TinkarTerm.PREFERRED)
                .definition("A concept indicating the operator \">=\"", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GREATER_THAN_OR_EQUAL_TO.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.GROUPING)
                .fullyQualifiedName("Grouping (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Grouping", TinkarTerm.PREFERRED)
                .definition("The grouping attached to the mapping", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GROUPING.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EXACT, TinkarTerm.PARTIAL), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.HEALTH_CONCEPT)
                .fullyQualifiedName("Health concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Health concept", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.HEALTH_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(IDENTIFIER_SOURCE)
                .fullyQualifiedName("Identifier Source", TinkarTerm.PREFERRED)
                .synonym("Identifier source", TinkarTerm.PREFERRED)
                .definition("An identifier used to label the identity of a unique component.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, IDENTIFIER_SOURCE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.INCLUSION_SET)
                .fullyQualifiedName("Inclusion set", TinkarTerm.PREFERRED)
                .synonym("Inclusion set", TinkarTerm.PREFERRED)
                .definition("A set of relationships that indicate something is has an inclusion. Not necessarily or sufficient but inclusive.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INCLUSION_SET.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .statedDefinition(List.of(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.INACTIVE_STATE)
                .fullyQualifiedName("Inactive state", TinkarTerm.PREFERRED)
                .synonym("Inactive", TinkarTerm.PREFERRED)
                .definition("Concept used to represent a status for components that are no longer active", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INACTIVE_STATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.STATUS_VALUE))
                .statedDefinition(List.of(TinkarTerm.STATUS_VALUE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.INFERRED_PREMISE_TYPE)
                .fullyQualifiedName("Inferred premise type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Inferred relationship / Inferred", TinkarTerm.PREFERRED)
                .definition("The axiom view following the application of the reasoner", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INFERRED_PREMISE_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.AXIOM_ORIGIN))
                .statedDefinition(List.of(TinkarTerm.AXIOM_ORIGIN))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.INSTANT_LITERAL)
                .fullyQualifiedName("Instant literal (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Instant literal", TinkarTerm.PREFERRED)
                .definition("May refer to a specific point in time which is often represented by a date or time value", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INSTANT_LITERAL.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LITERAL_VALUE))
                .statedDefinition(List.of(LITERAL_VALUE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.INSTANT_SUBSTITUTION)
                .fullyQualifiedName("Instant substitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Instant substitution", TinkarTerm.PREFERRED)
                .definition("Substitution of instant literal?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INSTANT_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(FIELD_SUBSTITUTION))
                .statedDefinition(List.of(FIELD_SUBSTITUTION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.INTEGER_FIELD)
                .fullyQualifiedName("Integer Field", TinkarTerm.PREFERRED)
                .synonym("Integer field", TinkarTerm.PREFERRED)
                .definition("Data type that represents some range of mathematical integers", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INTEGER_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.INVERSE_NAME)
                .fullyQualifiedName("Inverse name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Inverse name", TinkarTerm.PREFERRED)
                .definition("This is the extended description type that maye be attached to a description within a concept that defines as Association refex to signify that the referenced description  is the inverse of the association name", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INVERSE_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.INVERSE_TREE_LIST)
                .fullyQualifiedName("Inverse tree list (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Inverse tree list", TinkarTerm.PREFERRED)
                .definition("Inverse tree list", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INVERSE_TREE_LIST.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TREE_AMALGAM_PROPERTIES))
                .statedDefinition(List.of(TREE_AMALGAM_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.IRISH_DIALECT)
                .fullyQualifiedName("Irish dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Irish dialect", TinkarTerm.PREFERRED)
                .definition("Irish dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IRISH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(DIALECT_ASSEMBLAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.IRISH_LANGUAGE)
                .fullyQualifiedName("Irish language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Irish language", TinkarTerm.PREFERRED)
                .definition("Irish language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IRISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.IS_A)
                .fullyQualifiedName("Is-a", TinkarTerm.PREFERRED)
                .synonym("Is a", TinkarTerm.PREFERRED)
                .definition("Designates the parent child relationship", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.IS_A_INFERRED_NAVIGATION)
                .fullyQualifiedName("Is-a inferred navigation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Is-a inferred navigation", TinkarTerm.PREFERRED)
                .definition("Designates the parent child relationship following the application of the reasoner", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A_INFERRED_NAVIGATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.NAVIGATION))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.IS_A_STATED_NAVIGATION)
                .fullyQualifiedName("Is-a stated navigation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Is-a stated navigation", TinkarTerm.PREFERRED)
                .definition("Designates the parent child relationship as authored", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A_STATED_NAVIGATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.NAVIGATION))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ITALIAN_LANGUAGE)
                .fullyQualifiedName("Italian Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Italian language", TinkarTerm.PREFERRED)
                .definition("Italian language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ITALIAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.KOMET_MODULE)
                .fullyQualifiedName("KOMET module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("KOMET module", TinkarTerm.PREFERRED)
                .definition("Komet specific values?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODULE))
                .statedDefinition(List.of(TinkarTerm.MODULE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.KOMET_USER)
                .fullyQualifiedName("KOMET user (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("KOMET user", TinkarTerm.PREFERRED)
                .definition("Authorized to author, edit and/or view in Komet", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_USER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.KOMET_USER_LIST)
                .fullyQualifiedName("KOMET user list (SOLOR", TinkarTerm.PREFERRED)
                .synonym("KOMET user list", TinkarTerm.PREFERRED)
                .definition("Inventory of authorized komet users", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_USER_LIST.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.KOMET_ISSUE)
                .fullyQualifiedName("Komet issue (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Komet issue", TinkarTerm.PREFERRED)
                .definition("Komet being the 'annotation type' - specified type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_ISSUE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ANNOTATION_TYPE))
                .statedDefinition(List.of(TinkarTerm.ANNOTATION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.KOREAN_DIALECT)
                .fullyQualifiedName("Korean dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Korean dialect", TinkarTerm.PREFERRED)
                .definition("Korean dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOREAN_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.STANDARD_KOREAN_DIALECT), List.of(DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(DIALECT_ASSEMBLAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.KOREAN_LANGUAGE)
                .fullyQualifiedName("Korean Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Korean language", TinkarTerm.PREFERRED)
                .definition("Korean language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOREAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(LANGUAGE)
                .fullyQualifiedName("Language", TinkarTerm.PREFERRED)
                .synonym("Language", TinkarTerm.PREFERRED)
                .definition("Specifies the language of the description text.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ENGLISH_LANGUAGE, TinkarTerm.SPANISH_LANGUAGE), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION)
                .fullyQualifiedName("Language concept nid for description (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language for description", TinkarTerm.PREFERRED)
                .definition("Captures the language code for a description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_NAME)
                .fullyQualifiedName("Language coordinate name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language coordinate name", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_COORDINATE_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES)
                .fullyQualifiedName("Language coordinate properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language coordinate properties", TinkarTerm.PREFERRED)
                .definition("Spatial representation of language, attributes or language coordinates, programming language metadata?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.LANGUAGE_COORDINATE_NAME, DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE), List.of(OBJECT_PROPERTIES))
                .statedDefinition(List.of(OBJECT_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName("Language nid for language coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language nid", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName("Language specification for language coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LESS_THAN)
                .fullyQualifiedName("Less than (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Less than", TinkarTerm.PREFERRED)
                .definition("A concept indicating the operator \"<\"", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LESS_THAN.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LESS_THAN_OR_EQUAL_TO)
                .fullyQualifiedName("Less than or equal to (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Less than or equal to", TinkarTerm.PREFERRED)
                .definition("A concept indicating the operator \"<=\"", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LESS_THAN_OR_EQUAL_TO.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LITHUANIAN_LANGUAGE)
                .fullyQualifiedName("Lithuanian language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Lithuanian Language", TinkarTerm.PREFERRED)
                .definition("Lithuanian Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LITHUANIAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LOGIC_COORDINATE_NAME)
                .fullyQualifiedName("Logic coordinate name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logic coordinate name", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGIC_COORDINATE_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LOGIC_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.LOGIC_COORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LOGIC_COORDINATE_PROPERTIES)
                .fullyQualifiedName("Logic coordinate properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logic coordinate properties", TinkarTerm.PREFERRED)
                .definition("Structural characteristics of logical elements, Attributes of Logical coordinates, Mathematical Representation of logical relationships ?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGIC_COORDINATE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.LOGIC_COORDINATE_NAME), List.of(OBJECT_PROPERTIES))
                .statedDefinition(List.of(OBJECT_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LOGICAL_DEFINITION)
                .fullyQualifiedName("Logical Definition", TinkarTerm.PREFERRED)
                .synonym("Logical Definition", TinkarTerm.PREFERRED)
                .definition("The semantic value describing the purpose of the stated and inferred terminological axioms.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LOGICAL_EXPRESSION_FIELD)
                .fullyQualifiedName("Logical expression field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logical expression field", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_EXPRESSION_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DISPLAY_FIELDS))
                .statedDefinition(List.of(DISPLAY_FIELDS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC)
                .fullyQualifiedName("Logical expression semantic  (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logical expression semantic", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.LOGICALLY_EQUIVALENT_TO)
                .fullyQualifiedName("Logically equivalent to (Solor)", TinkarTerm.PREFERRED)
                .synonym("Logically equivalent to", TinkarTerm.PREFERRED)
                .definition("An operator for the reasoner to determine the equivalence", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICALLY_EQUIVALENT_TO.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TAXONOMY_OPERATOR))
                .statedDefinition(List.of(TAXONOMY_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MASTER_PATH)
                .fullyQualifiedName("Master path", TinkarTerm.PREFERRED)
                .synonym("Master path", TinkarTerm.PREFERRED)
                .definition("A default path for components", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MASTER_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH))
                .statedDefinition(List.of(TinkarTerm.PATH))
                .pathMembership()
                .pathOrigin(TinkarTerm.DEVELOPMENT_PATH)
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MEANING)
                .fullyQualifiedName("Meaning", TinkarTerm.PREFERRED)
                .synonym("Meaning", TinkarTerm.PREFERRED)
                .definition("The interpretation or explanation field for a pattern/semantics", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MEANING.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .fullyQualifiedName("Membership semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Membership semantic", TinkarTerm.PREFERRED)
                .definition("Membership semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MEMBERSHIP_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODEL_CONCEPT)
                .fullyQualifiedName("Model concept", TinkarTerm.PREFERRED)
                .synonym("Model concept", TinkarTerm.PREFERRED)
                .definition("", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODEL_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TINKAR_MODEL_CONCEPT), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TINKAR_MODEL_CONCEPT)
                .fullyQualifiedName("Tinkar Model concept", TinkarTerm.PREFERRED)
                .synonym("Tinkar Model concept", TinkarTerm.PREFERRED)
                .definition("", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TINKAR_MODEL_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.AXIOM_ORIGIN, TinkarTerm.CONCRETE_DOMAIN_OPERATOR, TinkarTerm.DESCRIPTION, TinkarTerm.DESCRIPTION_ACCEPTABILITY,
                        TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE, TinkarTerm.DESCRIPTION_SEMANTIC, TinkarTerm.DESCRIPTION_TYPE,
                        DIALECT_ASSEMBLAGE, DISPLAY_FIELDS, TinkarTerm.EL_PLUS_PLUS_TERMINOLOGICAL_AXIOMS,
                        IDENTIFIER_SOURCE, IDENTIFIER_VALUE, TinkarTerm.INFERRED_DEFINITION, TinkarTerm.IS_A,
                        LANGUAGE, TinkarTerm.LOGICAL_DEFINITION, TinkarTerm.MEANING, TinkarTerm.PURPOSE,
                        PHENOMENON, TinkarTerm.RELATIONSHIP_DESTINATION, TinkarTerm.RELATIONSHIP_ORIGIN,
                        REFERENCE_RANGE, STATED_DEFINITION, TinkarTerm.TEXT_FOR_DESCRIPTION, VALUE_CONSTRAINT,
                        VALUE_CONSTRAINT_SOURCE, TinkarTerm.AXIOM_SYNTAX),
                        List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULE)
                .fullyQualifiedName("Module", TinkarTerm.PREFERRED)
                .synonym("Module", TinkarTerm.PREFERRED)
                .definition("Null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.PRIMORDIAL_MODULE), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Module exclusion set for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module exclusions", TinkarTerm.PREFERRED)
                .definition("Module exclusion set for stamp coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULE_FOR_USER)
                .fullyQualifiedName("Module for user (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module for user", TinkarTerm.PREFERRED)
                .definition("User preference for Module?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_FOR_USER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULE_FOR_VERSION)
                .fullyQualifiedName("Module for version (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module", TinkarTerm.PREFERRED)
                .definition("Module Version", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_FOR_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(VERSION_PROPERTIES))
                .statedDefinition(List.of(VERSION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE)
                .fullyQualifiedName("Module options for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module options", TinkarTerm.PREFERRED)
                .definition("Coordinate edit options for Module", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName("Module preference list for language coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module nids", TinkarTerm.PREFERRED)
                .definition("Module preference list for language coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Module preference list for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module Preference list", TinkarTerm.PREFERRED)
                .definition("Module preference list for stamp coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Module preference order for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module order", TinkarTerm.PREFERRED)
                .definition("Module preference order for stamp coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.MODULES_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Modules for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Modules", TinkarTerm.PREFERRED)
                .definition("Stamp coordinate modules", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.NAVIGATION)
                .fullyQualifiedName("Navigation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Navigation", TinkarTerm.PREFERRED)
                .definition("Navigation", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.NAVIGATION_CONCEPT_SET, TinkarTerm.NAVIGATION_VERTEX), List.of(TinkarTerm.PURPOSE))
                .statedDefinition(List.of(TinkarTerm.PURPOSE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.NAVIGATION_CONCEPT_SET)
                .fullyQualifiedName("Navigation concept set (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Navigation set", TinkarTerm.PREFERRED)
                .definition("Navigating sets of concepts?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION_CONCEPT_SET.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.NAVIGATION))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.NAVIGATION_VERTEX)
                .fullyQualifiedName("Navigation vertex (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Navigation vertex", TinkarTerm.PREFERRED)
                .definition("Navigation vertex", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION_VERTEX.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.NAVIGATION))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION)
                .fullyQualifiedName("Necessary but not sufficient concept definition (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Necessary but not sufficient concept definition", TinkarTerm.PREFERRED)
                .definition("Not sufficiently defined by necessary conditions definition status", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.NECESSARY_SET)
                .fullyQualifiedName("Necessary set", TinkarTerm.PREFERRED)
                .synonym("Necessary set", TinkarTerm.PREFERRED)
                .definition("A set of relationships that is always true of a concept.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NECESSARY_SET.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .statedDefinition(List.of(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.NOT_APPLICABLE)
                .fullyQualifiedName("Not Applicable (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Not applicable", TinkarTerm.PREFERRED)
                .definition("Not available", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NOT_APPLICABLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(OBJECT)
                .fullyQualifiedName("Object (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Object", TinkarTerm.PREFERRED)
                .definition("An encapsulation of data together with procedures", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, OBJECT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.STATUS_VALUE, TinkarTerm.DESCRIPTION, TinkarTerm.NID, TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANY_COMPONENT, TinkarTerm.UNINITIALIZED_COMPONENT, TinkarTerm.SANDBOX_COMPONENT, TinkarTerm.MODULE, TinkarTerm.PATH, OBJECT_PROPERTIES, HAS_ACTIVE_INGREDIENT, HAS_DOSE_FORM, LATERALITY), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(OBJECT_PROPERTIES)
                .fullyQualifiedName("Object Properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Object properties", TinkarTerm.PREFERRED)
                .definition("Objects are instances of classes, the properties describe the data or attributes that an object can have", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, OBJECT_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(ACTION_PROPERTIES, CHRONICLE_PROPERTIES, VERSION_PROPERTIES, IMMUTABLECOORDINATE_PROPERTIES, TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES, TinkarTerm.LOGIC_COORDINATE_PROPERTIES, TinkarTerm.PATH_COORDINATE_PROPERTIES, TinkarTerm.SEMANTIC_PROPERTIES, TREE_AMALGAM_PROPERTIES, TinkarTerm.CORRELATION_PROPERTIES, TRANSITIVE_PROPERTY, REFLEXIVE_PROPERTY), List.of(OBJECT))
                .statedDefinition(List.of(OBJECT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.OR)
                .fullyQualifiedName("Or (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Or", TinkarTerm.PREFERRED)
                .definition("Operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.OR.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS)
                .fullyQualifiedName("Order for axiom attachments (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Axiom attachment order", TinkarTerm.PREFERRED)
                .definition("Order in which axioms are attached", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS)
                .fullyQualifiedName("Order for concept attachments  (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept attachment order", TinkarTerm.PREFERRED)
                .definition("Order in which concepts are attached", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS)
                .fullyQualifiedName("Order for description attachments (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description attachment order", TinkarTerm.PREFERRED)
                .definition("Order in which descriptions are attached", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PART_OF)
                .fullyQualifiedName("Part of (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Part of", TinkarTerm.PREFERRED)
                .definition("Part of an attribute", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PART_OF.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PARTIAL)
                .fullyQualifiedName("Partial (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Partial", TinkarTerm.PREFERRED)
                .definition("Exists in/ Inclusion of ?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PARTIAL.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.GROUPING))
                .statedDefinition(List.of(TinkarTerm.GROUPING))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH)
                .fullyQualifiedName("Path", TinkarTerm.PREFERRED)
                .synonym("Path", TinkarTerm.PREFERRED)
                .definition("A set of assets under version control that can be managed distinctly from other assets. Paths “branch” from other paths when established, and can be “merged” with other paths as well.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DEVELOPMENT_PATH, TinkarTerm.MASTER_PATH, TinkarTerm.PRIMORDIAL_PATH, TinkarTerm.SANDBOX_PATH), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_CONCEPT)
                .fullyQualifiedName("Path concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path concept", TinkarTerm.PREFERRED)
                .definition("Path concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCEPT_TYPE))
                .statedDefinition(List.of(TinkarTerm.CONCEPT_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_COORDINATE_NAME)
                .fullyQualifiedName("Path coordinate name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path coordinate name", TinkarTerm.PREFERRED)
                .definition("Path coordinate name", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_COORDINATE_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_COORDINATE_PROPERTIES)
                .fullyQualifiedName("Path coordinate properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path coordinate properties", TinkarTerm.PREFERRED)
                .definition("Character or attribute of coordinates referring to a series of connected points, that form a shape or trajectory", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_COORDINATE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.PATH_COORDINATE_NAME, TinkarTerm.PATH_ORIGINS), List.of(OBJECT_PROPERTIES))
                .statedDefinition(List.of(OBJECT_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_PATH_COORDINATE)
                .fullyQualifiedName("Path for path coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path", TinkarTerm.PREFERRED)
                .definition("Path for path coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_PATH_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_USER)
                .fullyQualifiedName("Path for user (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path for user", TinkarTerm.PREFERRED)
                .definition("Path for user", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_USER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_VERSION)
                .fullyQualifiedName("Path for version", TinkarTerm.PREFERRED)
                .synonym("Path", TinkarTerm.PREFERRED)
                .definition("Version path", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(VERSION_PROPERTIES))
                .statedDefinition(List.of(VERSION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE)
                .fullyQualifiedName("Path options for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path options", TinkarTerm.PREFERRED)
                .definition("Path options for edit coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_ORIGINS)
                .fullyQualifiedName("Path origins (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path origins", TinkarTerm.PREFERRED)
                .definition("Path origins", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_ORIGINS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH)
                .fullyQualifiedName("Path origins for stamp path (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path origins", TinkarTerm.PREFERRED)
                .definition("Path origins for stamp path", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PROMOTION_PATH_FOR_EDIT_CORDINATE)
                .fullyQualifiedName("Promotion Path for Edit Coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Promotion Path for Edit Coordinate", TinkarTerm.PREFERRED)
                .definition("Promotion Path for Edit Coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PROMOTION_PATH_FOR_EDIT_CORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(PHENOMENON)
                .fullyQualifiedName("Phenomenon", TinkarTerm.PREFERRED)
                .synonym("Phenomenon", TinkarTerm.PREFERRED)
                .definition("A unique thought, fact, or circumstance", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, PHENOMENON.asUuidArray()[0].toString())
                .statedNavigation(List.of(EXAMPLE_UCUM_UNITS), List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.POLISH_DIALECT)
                .fullyQualifiedName("Polish dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Polish dialect", TinkarTerm.PREFERRED)
                .definition("Polish Dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.POLISH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(DIALECT_ASSEMBLAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.POLISH_LANGUAGE)
                .fullyQualifiedName("Polish Language (Language)", TinkarTerm.PREFERRED)
                .synonym("Polish language", TinkarTerm.PREFERRED)
                .definition("Polish Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.POLISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(LANGUAGE))
                .statedDefinition(List.of(LANGUAGE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PREFERRED)
                .fullyQualifiedName("Preferred (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Preferred", TinkarTerm.PREFERRED)
                .definition("Preferred( Foundation metadata concept)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PREFERRED.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PRESENTATION_UNIT_DIFFERENT)
                .fullyQualifiedName("Presentation unit different (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Presentation unit different", TinkarTerm.PREFERRED)
                .definition("Unit difference", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRESENTATION_UNIT_DIFFERENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE)
                .fullyQualifiedName("Primordial UUID for chronicle (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Primordial UUID", TinkarTerm.PREFERRED)
                .definition("Primordial UUID", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(CHRONICLE_PROPERTIES))
                .statedDefinition(List.of(CHRONICLE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_MODULE)
                .fullyQualifiedName("Primordial module", TinkarTerm.PREFERRED)
                .synonym("Primordial module", TinkarTerm.PREFERRED)
                .definition("", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODULE))
                .statedDefinition(List.of(TinkarTerm.MODULE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_PATH)
                .fullyQualifiedName("Primordial path", TinkarTerm.PREFERRED)
                .synonym("Primordial path", TinkarTerm.PREFERRED)
                .definition("", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH))
                .statedDefinition(List.of(TinkarTerm.PATH))
                .pathMembership()
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_STATE)
                .fullyQualifiedName("Primordial state", TinkarTerm.PREFERRED)
                .synonym("Primordial", TinkarTerm.PREFERRED)
                .definition("Concept used to represent a status for components that have not yet been released and exist in their most basic form.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_STATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.STATUS_VALUE))
                .statedDefinition(List.of(TinkarTerm.STATUS_VALUE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC)
                .fullyQualifiedName("Referenced component nid for semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Referenced component id", TinkarTerm.PREFERRED)
                .definition("Component id Referenced", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION)
                .fullyQualifiedName("Referenced component subtype restriction (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Referenced component subtype restriction", TinkarTerm.PREFERRED)
                .definition("Stores the (optional) referenced component type sub restriction selection which will be used by the validator to check the user input for the referenced component when creating an instance of a dynamic field.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(ROLE_OPERATOR))
                .statedDefinition(List.of(ROLE_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION)
                .fullyQualifiedName("Referenced component type restriction (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Referenced component type restriction", TinkarTerm.PREFERRED)
                .definition("Stores the (Optional) referenced component type restriction selection which will be used by the validator to check the user input for the referenced component when creating an instance of a dynamic field", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(ROLE_OPERATOR))
                .statedDefinition(List.of(ROLE_OPERATOR))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE)
                .fullyQualifiedName("Regular name description type", TinkarTerm.PREFERRED)
                .synonym("Regular name description type", TinkarTerm.PREFERRED)
                .definition("There may be descriptions/synonyms marked as “regular.”", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.RELATIONSHIP_DESTINATION)
                .fullyQualifiedName("Relationship destination", TinkarTerm.PREFERRED)
                .synonym("Relationship destination", TinkarTerm.PREFERRED)
                .definition("Signifies path to child concepts which are more specific than the Tinkar term", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RELATIONSHIP_DESTINATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.RELATIONSHIP_ORIGIN)
                .fullyQualifiedName("Relationship origin", TinkarTerm.PREFERRED)
                .synonym("Relationship origin", TinkarTerm.PREFERRED)
                .definition("Signifies path to parent concepts which are more general than the Tinkar term", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RELATIONSHIP_ORIGIN.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TINKAR_MODEL_CONCEPT))
                .statedDefinition(List.of(TINKAR_MODEL_CONCEPT))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ROLE)
                .fullyQualifiedName("Role", TinkarTerm.PREFERRED)
                .synonym("Role", TinkarTerm.PREFERRED)
                .definition("Is an abstract representation of a high-level role for a therapeutic medicinal product; the concepts are not intended to describe a detailed indication for therapeutic use nor imply that therapeutic use is appropriate in all clinical situations.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ROLE_TYPE, ROLE_OPERATOR, TinkarTerm.ROLE_RESTRICTION), List.of(TinkarTerm.ROLE_GROUP, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .statedDefinition(List.of(TinkarTerm.ROLE_GROUP, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ROLE_GROUP)
                .fullyQualifiedName("Role group", TinkarTerm.PREFERRED)
                .synonym("Role group", TinkarTerm.PREFERRED)
                .definition("An association between a set of attribute or axiom value pairs that causes them to be considered together within a concept definition or post coordinated expression.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_GROUP.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ROLE), List.of(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .statedDefinition(List.of(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))

                .tinkarBaseModelMembership()
                .build();

        starterData.concept(ROLE_OPERATOR)
                .fullyQualifiedName("Role operator", TinkarTerm.PREFERRED)
                .synonym("Role operator", TinkarTerm.PREFERRED)
                .definition("Concept that is used to describe universal vs existential restrictions.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, ROLE_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.UNIVERSAL_RESTRICTION, EXISTENTIAL_RESTRICTION), List.of(TinkarTerm.ROLE))
                .statedDefinition(List.of(TinkarTerm.ROLE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ROLE_RESTRICTION)
                .fullyQualifiedName(TinkarTerm.ROLE_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .synonym("Role value", TinkarTerm.PREFERRED)
                .definition("Role restriction", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_RESTRICTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROLE))
                .statedDefinition(List.of(TinkarTerm.ROLE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ROLE_TYPE)
                .fullyQualifiedName("Role type", TinkarTerm.PREFERRED)
                .synonym("Role type", TinkarTerm.PREFERRED)
                .definition("Refers to a concept that represents a particular kind of relationship that can exist between two entities. It defines the specific function or responsibility that one entity plays in relation to another.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROLE))
                .statedDefinition(List.of(TinkarTerm.ROLE))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ROLE_TYPE_TO_ADD)
                .fullyQualifiedName("Role type to add (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Role type to add", TinkarTerm.PREFERRED)
                .definition("Action - add role type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_TYPE_TO_ADD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(ACTION_PROPERTIES))
                .statedDefinition(List.of(ACTION_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        starterData.concept(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName("Root for logic coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Root", TinkarTerm.PREFERRED)
                .definition("Logic coordinate root", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROOT_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(IMMUTABLECOORDINATE_PROPERTIES))
                .tinkarBaseModelMembership()
                .build();

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.RUSSIAN_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Russian dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Russian dialect")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Russian Dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.RUSSIAN_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(DIALECT_ASSEMBLAGE));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.RUSSIAN_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Russian language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Russian language")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Russian language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.RUSSIAN_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SANDBOX_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Sandbox component")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Sandbox component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SANDBOX_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.SANDBOX_MODULE, TinkarTerm.SANDBOX_PATH)
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT));


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
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Sandbox module")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Sandbox module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SANDBOX_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.SANDBOX_PATH_MODULE)
                        .parents(TinkarTerm.MODULE, TinkarTerm.SANDBOX_COMPONENT))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MODULE, TinkarTerm.SANDBOX_COMPONENT));


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
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Sandbox path")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("A path for components under testing.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SANDBOX_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.PATH))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.PATH));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SANDBOX_PATH_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sandbox path module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Sandbox Path module")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Sandbox path module")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SANDBOX_PATH_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SANDBOX_MODULE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SANDBOX_MODULE));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_FIELD_CONCEPTS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field concepts (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Semantic field concepts")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Semantic field concepts")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_FIELD_CONCEPTS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.CONCEPT_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.CONCEPT_TYPE));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_FIELD_NAME))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field name (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Field name")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Field name - semantics")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_FIELD_NAME.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SEMANTIC_PROPERTIES));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_FIELD_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic field type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Semantic field type")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("List of fields-  semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_FIELD_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DISPLAY_FIELDS));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Semantic properties")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The attributes or characteristics of a concept, term, or element that convey meaning or semantics in a given context")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC, TinkarTerm.COMPONENT_FOR_SEMANTIC, TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC, TinkarTerm.SEMANTIC_FIELD_NAME)
                        .parents(TinkarTerm.OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.OBJECT_PROPERTIES));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Semantic type")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Type- semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.COMPONENT_SEMANTIC, TinkarTerm.CONCEPT_SEMANTIC, TinkarTerm.DESCRIPTION_SEMANTIC, TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC, TinkarTerm.MEMBERSHIP_SEMANTIC)
                        .parents(TinkarTerm.MEANING))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MEANING));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SIGNED_INTEGER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Signed integer (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Signed integer")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Signed integer (Foundation metadata concept)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SIGNED_INTEGER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SPANISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Spanish language")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Spanish language")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Value for the description language dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SPANISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.LANGUAGE));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STANDARD_KOREAN_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Standard Korean dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Standard Korean Dialect")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Standard")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STANDARD_KOREAN_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.KOREAN_DIALECT))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.KOREAN_DIALECT));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STATED_PREMISE_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated premise type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Stated")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Stated relationship")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STATED_PREMISE_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.AXIOM_ORIGIN))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.AXIOM_ORIGIN));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STATUS_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Status for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Status for version")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Version status?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STATUS_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.VERSION_PROPERTIES));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STATUS_VALUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Status value")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Status")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The status of the STAMP Coordinate(Active, Cancelled, Inactive, Primordial)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STATUS_VALUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ACTIVE_STATE, TinkarTerm.CANCELED_STATE, TinkarTerm.INACTIVE_STATE, TinkarTerm.PRIMORDIAL_STATE, TinkarTerm.WITHDRAWN_STATE)
                        .parents(TinkarTerm.ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.ROOT_VERTEX));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.STRING))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("String")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("String")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("A sequence of characters, either as a literal constant or as a variable. Strings could be used to represent terms from code systems or URLs, textual definitions, etc.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.STRING.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DISPLAY_FIELDS));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient concept definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Sufficient concept definition")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Concept definition - Sufficient")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient concept definition operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Sufficient concept definition operator")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Concept definition operator")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION, TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION)
                        .parents(TinkarTerm.MEANING))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MEANING));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SUFFICIENT_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Sufficient set")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Sufficient set")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("A set of relationships that differentiate a concept and its subtypes from all other concepts. A concept that contains at least one set of necessary and sufficient conditions is considered defined.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SUFFICIENT_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SWEDISH_LANGUAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Swedish language (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Swedish language")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Swedish Language")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SWEDISH_LANGUAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(LANGUAGE))
                .attach(new StatedAxiom()
                        .isA(LANGUAGE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.TEXT_FOR_DESCRIPTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Text for description")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Text")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Captures the human readable text for a description in Komet")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.TEXT_FOR_DESCRIPTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.TIME_FOR_VERSION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Time for version (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Time for version")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Version time")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.TIME_FOR_VERSION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(VERSION_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(VERSION_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TREE_AMALGAM_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Tree amalgam properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Tree amalgam properties")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Data structure that consists of nodes connected by edges (a mixture or blend of different elements)")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TREE_AMALGAM_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.INVERSE_TREE_LIST, TinkarTerm.TREE_LIST)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.TREE_LIST))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Tree list (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Tree list")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("List - Tree")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.TREE_LIST.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TREE_AMALGAM_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TREE_AMALGAM_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.US_ENGLISH_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("United States of America English dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("US English dialect")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("USA -english dialect")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.US_ENGLISH_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(US_NURSING_DIALECT)
                        .parents(ENGLISH_DIALECT_ASSEMBLAGE))
                .attach(new StatedAxiom()
                        .isA(ENGLISH_DIALECT_ASSEMBLAGE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(US_NURSING_DIALECT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("US Nursing dialect (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("United States English Nursing Dialect")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Nursing Dialect -US English")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(US_NURSING_DIALECT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(US_ENGLISH_DIALECT))
                .attach(new StatedAxiom()
                        .isA(US_ENGLISH_DIALECT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UUID_DATA_TYPE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID data type (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("UUID data type")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Distinction of data type of UUID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UUID_DATA_TYPE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UUID_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("UUID field")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Universally unique identifier field")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UUID_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UUID_LIST_FOR_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UUID list for component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("UUIDs")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("UUIDs")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UUID_LIST_FOR_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UNCATEGORIZED_PHENOMENON))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Uncategorized phenomenon (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Uncategorized phenomenon")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Unknown")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UNCATEGORIZED_PHENOMENON.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PHENOMENON))
                .attach(new StatedAxiom()
                        .isA(PHENOMENON));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UNINITIALIZED_COMPONENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Uninitialized Component (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Uninitialized")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Not initialized component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UNINITIALIZED_COMPONENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UNIVERSAL_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Universal Restriction")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Universal Restriction")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Universal restrictions constrain the relationships along a given property to concepts that are members of a specific class.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UNIVERSAL_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(ROLE_OPERATOR));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("UNIVERSALLY_UNIQUE_IDENTIFIER")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("UUID")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("A universally unique identifier that uniquely represents a concept in Tinkar")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IDENTIFIER_SOURCE))
                .attach(new StatedAxiom()
                        .isA(IDENTIFIER_SOURCE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.USER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Author")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Author")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.USER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.KOMET_USER, TinkarTerm.KOMET_USER_LIST, TinkarTerm.MODULE_FOR_USER, TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS, TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS, TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS, TinkarTerm.PATH_FOR_USER, TinkarTerm.STARTER_DATA_AUTHORING)
                        .parents(ROOT_VERTEX))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VERSION_LIST_FOR_CHRONICLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Version list for chronicle (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Versions")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Chronicle version list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VERSION_LIST_FOR_CHRONICLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VERSION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Version Properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Version properties")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Null")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VERSION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.AUTHOR_FOR_VERSION, TinkarTerm.MODULE_FOR_VERSION, TinkarTerm.PATH_FOR_VERSION, TinkarTerm.STATUS_FOR_VERSION, TinkarTerm.TIME_FOR_VERSION, TinkarTerm.DESCRIPTION_VERSION_PROPERTIES)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VERTEX_FIELD))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex field (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Vertex")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Field for Vertex")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VERTEX_FIELD.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DISPLAY_FIELDS))
                .attach(new StatedAxiom()
                        .isA(DISPLAY_FIELDS));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VERTEX_STATE_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex state set (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Vertex states")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Vertex states")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VERTEX_STATE_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VERTEX_SORT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Vertex sort (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Sort")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Vertex sort")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VERTEX_SORT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.VIEW_COORDINATE_KEY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("View coordinate key (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("View Key")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("View Key")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.VIEW_COORDINATE_KEY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.QUERY_CLAUSES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.QUERY_CLAUSES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.WITHDRAWN_STATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Withdrawn state")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Withdrawn")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Concept used to represent a status for components that are withdrawn.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.WITHDRAWN_STATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.STATUS_VALUE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.STATUS_VALUE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BOOLEAN))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Boolean (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Boolean")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BOOLEAN.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.BYTE_ARRAY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Byte array (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Byte array")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.BYTE_ARRAY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Description list for concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Description list for concept")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("List of description")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.DOUBLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Double (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Double")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.DOUBLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.FLOAT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Float (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Float")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.FLOAT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Logic graph for semantic (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Logic graph")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Semantic")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.SEMANTIC_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.SEMANTIC_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.LONG))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Long (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Long")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.LONG.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(DYNAMIC_COLUMN_DATA_TYPES))
                .attach(new StatedAxiom()
                        .isA(DYNAMIC_COLUMN_DATA_TYPES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.NID))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("NID (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Native Identifier")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Data type")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.NID.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Semantic list for chronicle (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Semantic list for chronicle")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Semantic list")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(CHRONICLE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(CHRONICLE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(USERS_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Users module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("User module")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Module - user")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(USERS_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.MODULE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MODULE));

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
                        .children(TinkarTerm.MODEL_CONCEPT, TinkarTerm.MEANING, OBJECT, TinkarTerm.ROLE, TinkarTerm.USER, TinkarTerm.ANNOTATION_TYPE, TinkarTerm.CREATIVE_COMMONS_BY_LICENSE, TinkarTerm.HEALTH_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(ROOT_VERTEX));

        //Necessary Terms Filtered out in generation routine - START
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(QUERY_CLAUSES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Query clauses (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Query clauses")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("A distinct component/query that serves a specific purpose")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(QUERY_CLAUSES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.BOOLEAN_REFERENCE, TinkarTerm.VIEW_COORDINATE_KEY)
                        .parents(TinkarTerm.MEANING))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MEANING));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FIELD_SUBSTITUTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Field substitution (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Field substitution")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Replacing a placeholder variable in a field with a specific value")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FIELD_SUBSTITUTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.BOOLEAN_SUBSTITUTION, TinkarTerm.CONCEPT_SUBSTITUTION, TinkarTerm.FLOAT_SUBSTITUTION, TinkarTerm.INSTANT_SUBSTITUTION)
                        .parents(TinkarTerm.MEANING))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MEANING));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TAXONOMY_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Taxonomy operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Taxonomy operator")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("An operator or set of operations applied within the context of a taxonomy")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TAXONOMY_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.LOGICALLY_EQUIVALENT_TO)
                        .parents(TinkarTerm.MEANING))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MEANING));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IMMUTABLECOORDINATE_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("ImmutableCoordinate Properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("ImmutableCoordinate properties")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("A set of values or data representing a point in space that one established cannot be changed?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IMMUTABLECOORDINATE_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE, TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE, TinkarTerm.MODULES_FOR_STAMP_COORDINATE, TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE, TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE, TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE, TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE, TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE, TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE, TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE, TinkarTerm.ROOT_FOR_LOGIC_COORDINATE, TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE, TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE, TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE, TinkarTerm.PATH_FOR_PATH_COORDINATE, TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH, TinkarTerm.VERTEX_SORT, TinkarTerm.VERTEX_STATE_SET, STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE, INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE, CLASSIFIER_FOR_LOGIC_COORDINATE, TinkarTerm.POSITION_ON_PATH)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PURPOSE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Purpose")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Purpose")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The reason for which a Tinkar value in a pattern was created or for which it exist.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TinkarTerm.PURPOSE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(ACTION_PROPERTIES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Action properties (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Action properties")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Attributes of an action object")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(ACTION_PROPERTIES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.CONCEPT_CONSTRAINTS, TinkarTerm.CONCEPT_TO_FIND, TinkarTerm.ROLE_TYPE_TO_ADD, TinkarTerm.CONDITIONAL_TRIGGERS)
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LITERAL_VALUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Literal value (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Literal value")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Fixed Value/Constant?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LITERAL_VALUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.BOOLEAN_LITERAL, TinkarTerm.FLOAT_LITERAL, TinkarTerm.INSTANT_LITERAL)
                        .parents(TinkarTerm.MEANING))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MEANING));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIALECT_ASSEMBLAGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dialect")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Dialect")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Specifies the dialect of the language.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIALECT_ASSEMBLAGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.CZECH_DIALECT, ENGLISH_DIALECT_ASSEMBLAGE, TinkarTerm.FRENCH_DIALECT, TinkarTerm.IRISH_DIALECT, TinkarTerm.KOREAN_DIALECT, TinkarTerm.POLISH_DIALECT, TinkarTerm.RUSSIAN_DIALECT)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DYNAMIC_COLUMN_DATA_TYPES))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dynamic column data types (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Dynamic column data types")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Data storage system where the structure of the data can be altered or extended dynamically?")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DYNAMIC_COLUMN_DATA_TYPES.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(TinkarTerm.ARRAY, TinkarTerm.BOOLEAN, TinkarTerm.BYTE_ARRAY, TinkarTerm.DOUBLE, TinkarTerm.FLOAT, TinkarTerm.LONG, TinkarTerm.SIGNED_INTEGER, TinkarTerm.STRING, TinkarTerm.UUID_DATA_TYPE)
                        .parents(TinkarTerm.MEANING))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.MEANING));

        //Necessary Terms Filtered out in generation routine - END

        // TODO: Get coordinates to work via Komet's KometTerm
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(POSITION_ON_PATH))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(POSITION_ON_PATH.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text(POSITION_ON_PATH.description())
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(POSITION_ON_PATH.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(POSITION_ON_PATH.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Stated assemblage")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Inferred assemblage")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Concepts to classify")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(CLASSIFIER_FOR_LOGIC_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(CLASSIFIER_FOR_LOGIC_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Classifier")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(CLASSIFIER_FOR_LOGIC_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CLASSIFIER_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IMMUTABLECOORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(IMMUTABLECOORDINATE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Dialect order")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SOLOR_OVERLAY_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("SOLOR overlay module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("SOLOR overlay module")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(SOLOR_OVERLAY_MODULE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SOLOR_OVERLAY_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SOLOR_MODULE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("SOLOR module (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("SOLOR module")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(SOLOR_MODULE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SOLOR_MODULE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(TRANSITIVE_PROPERTY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Transitive Feature (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Transitive Feature")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(TRANSITIVE_PROPERTY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(TRANSITIVE_PROPERTY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFLEXIVE_PROPERTY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Reflexive Feature (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Reflexive Feature")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(REFLEXIVE_PROPERTY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFLEXIVE_PROPERTY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT_PROPERTIES))
                .attach(new StatedAxiom()
                        .isA(OBJECT_PROPERTIES));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(LATERALITY))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Laterality (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Laterality")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(LATERALITY.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(LATERALITY.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(HAS_ACTIVE_INGREDIENT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Has Active Ingredient (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Has Active Ingredient")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(HAS_ACTIVE_INGREDIENT.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(HAS_ACTIVE_INGREDIENT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(HAS_DOSE_FORM))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Has Dose Form (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Has Dose Form")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(HAS_DOSE_FORM.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(HAS_DOSE_FORM.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(OBJECT))
                .attach(new StatedAxiom()
                        .isA(OBJECT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(UNMODELED_ROLE_CONCEPT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Unmodeled role concept (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Unmodeled role concept")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(UNMODELED_ROLE_CONCEPT.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(UNMODELED_ROLE_CONCEPT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(DYNAMIC_REFERENCED_COMPONENT_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Dynamic referenced component restriction (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Dynamic referenced component restriction")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(DYNAMIC_REFERENCED_COMPONENT_RESTRICTION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(DYNAMIC_REFERENCED_COMPONENT_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EXISTENTIAL_RESTRICTION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Existential restriction")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Existential restriction")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Existential restrictions describe objects that participate in at least one relationship along a specified property to objects of a specified class.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EXISTENTIAL_RESTRICTION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(ROLE_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(ROLE_OPERATOR));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INTRINSIC_ROLE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Intrinsic role (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Intrinsic role")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(INTRINSIC_ROLE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INTRINSIC_ROLE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PROPERTY_PATTERN_IMPLICATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Property pattern implication (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Property pattern implication")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(PROPERTY_PATTERN_IMPLICATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PROPERTY_PATTERN_IMPLICATION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(SNOROCKET_CLASSIFIER))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("SnoRocket classifier (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("SnoRocket classifier")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(SNOROCKET_CLASSIFIER.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(SNOROCKET_CLASSIFIER.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(PROPERTY_SET))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Property set (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Property set")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(PROPERTY_SET.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(PROPERTY_SET.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(FEATURE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Feature (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Feature")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(FEATURE.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(FEATURE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(EXAMPLE_UCUM_UNITS))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Example UCUM Units (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Example UCUM Units")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The Unified Code for Units of Measure (UCUM) is a code system intended to " +
                                "include all units of measures being contemporarily used in international science, " +
                                "engineering, and business. (www.unitsofmeasure.org) This field contains example units " +
                                "of measures for this term expressed as UCUM units.")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(EXAMPLE_UCUM_UNITS.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(PHENOMENON))
                .attach(new StatedAxiom()
                        .isA(PHENOMENON));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INFERRED_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inferred Definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Inferred Definition")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The relationships/axioms of a concept that have been inferred")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(INFERRED_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(IDENTIFIER_VALUE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Identifier Value (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Identifier Value")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The literal string value identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(IDENTIFIER_VALUE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MAXIMUM_VALUE_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Maximum Value Operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Maximum Value Operator; Maximum Domain Operator")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The Value Operator assigned to the Maximum Value in a Range")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MAXIMUM_VALUE_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.CONCRETE_DOMAIN_OPERATOR));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(MINIMUM_VALUE_OPERATOR))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Minimum Value Operator (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Minimum Value Operator; Minimum Domain Operator")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The Value Operator assigned to the Minimum Value in a Range")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(MINIMUM_VALUE_OPERATOR.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.CONCRETE_DOMAIN_OPERATOR));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCE_RANGE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Reference Range (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Value Range")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The range of values specific to a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCE_RANGE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .children(REFERENCE_RANGE_MAXIMUM, REFERENCE_RANGE_MINIMUM)
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));


        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCE_RANGE_MAXIMUM))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Reference Range Maximum (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Maximum Value; Max Value")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The highest possible value for a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCE_RANGE_MAXIMUM.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(REFERENCE_RANGE))
                .attach(new StatedAxiom()
                        .isA(REFERENCE_RANGE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(REFERENCE_RANGE_MINIMUM))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Reference Range Minimum (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Minimum Value; Min Value")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The lowest possible value for a component")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(REFERENCE_RANGE_MINIMUM.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(REFERENCE_RANGE))
                .attach(new StatedAxiom()
                        .isA(REFERENCE_RANGE));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATED_DEFINITION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated Definition (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Stated Definition")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("Relationships/Axioms of a concept that have been explicitly stated and defined")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(STATED_DEFINITION.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VALUE_CONSTRAINT))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Value Constraint (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Value Constraint")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("A component has specific value requirements that needs to be met")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VALUE_CONSTRAINT.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(VALUE_CONSTRAINT_SOURCE))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Value Constraint Source (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Value Constraint Source")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text("The source organization of that specifies the constraint")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(VALUE_CONSTRAINT_SOURCE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TINKAR_MODEL_CONCEPT))
                .attach(new StatedAxiom()
                        .isA(TINKAR_MODEL_CONCEPT));


        //Create Description Pattern
        starterData.pattern(TinkarTerm.DESCRIPTION_PATTERN)
                .fullyQualifiedName("Description Pattern", TinkarTerm.PREFERRED)
                .synonym("Description Pattern", TinkarTerm.PREFERRED)
                .definition("Contains all metadata and human readable text that describes the concept", TinkarTerm.PREFERRED)
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
                        TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE,
                        TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.DESCRIPTION_TYPE,
                        TinkarTerm.DESCRIPTION_TYPE,
                        TinkarTerm.COMPONENT_FIELD)
                .tinkarBaseModelMembership()
                .build();

        //Create Stated Navigation Pattern
        starterData.pattern(TinkarTerm.STATED_NAVIGATION_PATTERN)
                .fullyQualifiedName("Stated Navigation Pattern", TinkarTerm.PREFERRED)
                .synonym("Stated Navigation Pattern", TinkarTerm.PREFERRED)
                .definition("A pattern specifying the origins and destinations for concepts based on the stated terminological axioms.", TinkarTerm.PREFERRED)
                .meaning(TinkarTerm.IS_A)
                .purpose(TinkarTerm.IS_A)
                .fieldDefinition(
                        TinkarTerm.RELATIONSHIP_DESTINATION,
                        TinkarTerm.IS_A,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fieldDefinition(
                        TinkarTerm.RELATIONSHIP_ORIGIN,
                        TinkarTerm.IS_A,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .tinkarBaseModelMembership()
                .build();

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(STATED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Stated navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Stated navigation")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(STATED_NAVIGATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));


        //Create Inferred Navigation Pattern
        starterData.pattern(TinkarTerm.INFERRED_NAVIGATION_PATTERN)
                .fullyQualifiedName("Inferred Navigation Pattern", TinkarTerm.PREFERRED)
                .synonym("Inferred Navigation Pattern", TinkarTerm.PREFERRED)
                .definition("A pattern specifying the origins and destinations for concepts based on the inferred terminological axioms.", TinkarTerm.PREFERRED)
                .meaning(TinkarTerm.IS_A)
                .purpose(TinkarTerm.IS_A)
                .fieldDefinition(
                        TinkarTerm.RELATIONSHIP_DESTINATION,
                        TinkarTerm.IS_A,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fieldDefinition(
                        TinkarTerm.RELATIONSHIP_ORIGIN,
                        TinkarTerm.IS_A,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .tinkarBaseModelMembership()
                .build();

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(INFERRED_NAVIGATION))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("Inferred navigation (SOLOR)")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(new USDialect()
                                .acceptability(PREFERRED))
                        .attach((Synonym synonym) -> synonym
                                .text("Inferred navigation")
                                .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                                .language(ENGLISH_LANGUAGE)))
                .attach(new USDialect()
                        .acceptability(PREFERRED))
                .attach((Definition definition) -> definition
                        .text(INFERRED_NAVIGATION.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE))
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(TinkarTerm.DESCRIPTION_TYPE))
                .attach(new StatedAxiom()
                        .isA(TinkarTerm.DESCRIPTION_TYPE));


        //Create Identifier Pattern
        starterData.pattern(TinkarTerm.IDENTIFIER_PATTERN)
                .fullyQualifiedName("Identifier Pattern", TinkarTerm.PREFERRED)
                .synonym("Identifier Pattern", TinkarTerm.PREFERRED)
                .definition("An identifier pattern is used to identity a concept which contains the identifier source and the actual value.", TinkarTerm.PREFERRED)
                .meaning(IDENTIFIER_SOURCE)
                .purpose(IDENTIFIER_SOURCE)
                .fieldDefinition(
                        IDENTIFIER_SOURCE,
                        IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        IDENTIFIER_VALUE,
                        IDENTIFIER_VALUE,
                        TinkarTerm.STRING)
                .tinkarBaseModelMembership()
                .build();

        //Create US Dialect Pattern
        starterData.pattern(TinkarTerm.US_DIALECT_PATTERN)
                .fullyQualifiedName("US Dialect Pattern", TinkarTerm.PREFERRED)
                .synonym("US Dialect Pattern", TinkarTerm.PREFERRED)
                .meaning(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fieldDefinition(
                        TinkarTerm.US_ENGLISH_DIALECT,
                        TinkarTerm.DESCRIPTION_ACCEPTABILITY,
                        TinkarTerm.COMPONENT_FIELD)
                .tinkarBaseModelMembership()
                .build();

        //Create GB Dialect Pattern
        starterData.pattern(TinkarTerm.GB_DIALECT_PATTERN)
                .fullyQualifiedName("GB Dialect Pattern", TinkarTerm.PREFERRED)
                .synonym("GB Dialect Pattern", TinkarTerm.PREFERRED)
                .definition("Particular form of language specific form of English language, particular to Great Britain.", TinkarTerm.PREFERRED)
                .meaning(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fieldDefinition(
                        TinkarTerm.GB_ENGLISH_DIALECT,
                        TinkarTerm.DESCRIPTION_ACCEPTABILITY,
                        TinkarTerm.COMPONENT_FIELD)
                .tinkarBaseModelMembership()
                .build();

        //Create Axiom Syntax Pattern
        starterData.pattern(TinkarTerm.OWL_AXIOM_SYNTAX_PATTERN)
                .meaning(TinkarTerm.AXIOM_SYNTAX)
                .purpose(TinkarTerm.EXPRESS_AXIOM_SYNTAX)
                .fieldDefinition(
                        TinkarTerm.AXIOM_SYNTAX,
                        TinkarTerm.EXPRESS_AXIOM_SYNTAX,
                        TinkarTerm.STRING)
                .tinkarBaseModelMembership()
                .build();

        //Create Stated Definition Pattern
        starterData.pattern(TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN)
                .fullyQualifiedName("EL++ Stated Axioms Pattern", TinkarTerm.PREFERRED)
                .synonym("Stated definition pattern", TinkarTerm.PREFERRED)
                .meaning(STATED_DEFINITION)
                .purpose(TinkarTerm.LOGICAL_DEFINITION)
                .fieldDefinition(
                        TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS,
                        TinkarTerm.LOGICAL_DEFINITION,
                        TinkarTerm.DITREE_FIELD)
                .tinkarBaseModelMembership()
                .build();

        //Create Inferred Definition Pattern
        starterData.pattern(TinkarTerm.EL_PLUS_PLUS_INFERRED_AXIOMS_PATTERN)
                .fullyQualifiedName("EL++ Inferred Axioms Pattern", TinkarTerm.PREFERRED)
                .synonym("Inferred definition pattern", TinkarTerm.PREFERRED)
                .meaning(TinkarTerm.INFERRED_DEFINITION)
                .purpose(TinkarTerm.LOGICAL_DEFINITION)
                .fieldDefinition(
                        TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS,
                        TinkarTerm.LOGICAL_DEFINITION,
                        TinkarTerm.DITREE_FIELD)
                .tinkarBaseModelMembership()
                .build();

        //Create Path Membership Pattern
        starterData.pattern(TinkarTerm.PATHS_PATTERN)
                .meaning(TinkarTerm.PATH)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .tinkarBaseModelMembership()
                .build();

        starterData.pattern(TinkarTerm.SOLOR_CONCEPT_ASSEMBLAGE)
                .meaning(CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .tinkarBaseModelMembership()
                .build();

        //Create STAMP Pattern
        starterData.pattern(TinkarTerm.STAMP_PATTERN)
                .meaning(VERSION_PROPERTIES)
                .purpose(VERSION_PROPERTIES)
                .fieldDefinition(
                        TinkarTerm.STATUS_VALUE,
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
                        TinkarTerm.COMPONENT_FIELD)
                .tinkarBaseModelMembership()
                .build();

        starterData.pattern(TinkarTerm.PATH_ORIGINS_PATTERN)
                .fullyQualifiedName("Path origins pattern (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path origins pattern", TinkarTerm.PREFERRED)
                .definition("Pattern of path origins", TinkarTerm.PREFERRED)
                .meaning(TinkarTerm.PATH_ORIGINS)
                .purpose(TinkarTerm.PATH_ORIGINS)
                .fieldDefinition(
                        TinkarTerm.PATH_CONCEPT,
                        TinkarTerm.PATH_CONCEPT,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.PATH_ORIGINS,
                        TinkarTerm.PATH_ORIGINS,
                        TinkarTerm.INSTANT_LITERAL)
                .tinkarBaseModelMembership()
                .build();

        //Create Comment Pattern
        starterData.pattern(TinkarTerm.COMMENT_PATTERN)
                .meaning(TinkarTerm.COMMENT)
                .purpose(TinkarTerm.COMMENT)
                .fieldDefinition(
                        TinkarTerm.COMMENT,
                        TinkarTerm.COMMENT,
                        TinkarTerm.STRING)
                .tinkarBaseModelMembership()
                .build();

        //Create Tinkar Core base model pattern
        starterData.pattern(TinkarTerm.TINKAR_BASE_MODEL_COMPONENT_PATTERN)
                .meaning(TinkarTerm.STARTER_DATA_AUTHORING)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .tinkarBaseModelMembership()
                .build();

        //Create Komet base model component pattern
        starterData.pattern(TinkarTerm.KOMET_BASE_MODEL_COMPONENT_PATTERN)
                .meaning(TinkarTerm.STARTER_DATA_AUTHORING)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .kometBaseModelMembership()
                .build();

        starterData.pattern(TinkarTerm.VALUE_CONSTRAINT_PATTERN)
                .fullyQualifiedName("Value Constraint Pattern", TinkarTerm.PREFERRED)
                .synonym("Value Constraint Pattern", TinkarTerm.PREFERRED)
                .definition("A pattern specifying value constraint pattern", TinkarTerm.PREFERRED)
                .meaning(VALUE_CONSTRAINT)
                .purpose(VALUE_CONSTRAINT)
                .fieldDefinition(
                        VALUE_CONSTRAINT_SOURCE,
                        VALUE_CONSTRAINT_SOURCE,
                        TinkarTerm.CONCEPT_FIELD)
                .fieldDefinition(
                        MINIMUM_VALUE_OPERATOR,
                        TinkarTerm.CONCRETE_DOMAIN_OPERATOR,
                        TinkarTerm.CONCEPT_FIELD)
                .fieldDefinition(
                        REFERENCE_RANGE_MINIMUM,
                        REFERENCE_RANGE,
                        TinkarTerm.FLOAT_FIELD)
                .fieldDefinition(
                        MAXIMUM_VALUE_OPERATOR,
                        TinkarTerm.CONCRETE_DOMAIN_OPERATOR,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        REFERENCE_RANGE_MAXIMUM,
                        REFERENCE_RANGE,
                        TinkarTerm.FLOAT_FIELD)
                .fieldDefinition(
                        EXAMPLE_UCUM_UNITS,
                        EXAMPLE_UCUM_UNITS,
                        TinkarTerm.STRING)
                .tinkarBaseModelMembership()
                .build();

        composer.commitSession(session);
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
