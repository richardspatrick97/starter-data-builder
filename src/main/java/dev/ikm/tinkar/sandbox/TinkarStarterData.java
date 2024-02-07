package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.io.FileUtil;
import dev.ikm.tinkar.entity.Entity;
import dev.ikm.tinkar.entity.EntityVersion;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.entity.load.LoadEntitiesFromProtobufFile;
import dev.ikm.tinkar.entity.transfom.EntityToTinkarSchemaTransformer;
import dev.ikm.tinkar.entity.transfom.TinkarSchemaToEntityTransformer;
import dev.ikm.tinkar.schema.TinkarMsg;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.StarterDataTerm;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;


public class TinkarStarterData {

    private static final Logger LOG = Logger.getLogger(TinkarStarterData.class.getSimpleName());

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
                        System.currentTimeMillis(),
                        TinkarTerm.USER,
                        TinkarTerm.PRIMORDIAL_MODULE,
                        TinkarTerm.PRIMORDIAL_PATH);

        authoringSTAMP = starterData.getAuthoringSTAMP();

        configureConceptsAndPatterns(starterData, uuidUtility);
        starterData.build(); //Natively writing data to spined array
        transformAnalysis(uuidUtility); //Isolate and inspect import and export transforms
        exportStarterData();  //exports starter data to pb.zip
        starterData.shutdown();

        //Load exported starter data into clean database
        importStarterData(); //load pb.zip into database

    }

    private static void configureConceptsAndPatterns(StarterData starterData, UUIDUtility uuidUtility){

        Concept uncategorizedGrouper = EntityProxy.Concept.make("UNCATEGORIZED_GROUPER", uuidUtility.createUUID());
        starterData.concept(uncategorizedGrouper)
                .fullyQualifiedName("UNCATEGORIZED_GROUPER", TinkarTerm.PREFERRED)
                .synonym("UNCATEGORIZED_GROUPER", TinkarTerm.PREFERRED)
                .definition("UNCATEGORIZED_GROUPER", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, uncategorizedGrouper.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        Concept englishDialect = EntityProxy.Concept.make("ENGLISH_DIALECT", UUID.randomUUID());
        starterData.concept(englishDialect)
                .fullyQualifiedName("English Dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("English dialect", TinkarTerm.PREFERRED)
                .definition("Dialect/ grouping", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, englishDialect.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.GB_ENGLISH_DIALECT, TinkarTerm.US_ENGLISH_DIALECT), List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .build();

        Concept textComparison = EntityProxy.Concept.make("TEXT_COMPARISON", UUID.randomUUID());
        starterData.concept(textComparison)
                .fullyQualifiedName("Text comparison measure semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Text comparsion", TinkarTerm.PREFERRED)
                .definition("Text comparison with a focus on semantic meaning involves evaluating the similarity or relatedness between pieces of text based on their underlying meaning rather than just their surface structure.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, textComparison.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CASE_INSENSITIVE_EVALUATION, TinkarTerm.CASE_SENSITIVE_EVALUATION), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        Concept starterDataAuthoring = EntityProxy.Concept.make("Starter Data Authoring", uuidUtility.createUUID());
        starterData.concept(starterDataAuthoring)
                .fullyQualifiedName(starterDataAuthoring.description(), TinkarTerm.PREFERRED)
                .synonym("Metadata Authoring", TinkarTerm.ACCEPTABLE)
                .definition("Define necessary minimum viable concepts to use Tinkar Data", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, starterDataAuthoring.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .build();

        Concept axiomSyntax = Concept.make("Axiom Syntax", uuidUtility.createUUID("Axiom Syntax"));
        starterData.concept(axiomSyntax)
                .fullyQualifiedName(axiomSyntax.description(), TinkarTerm.PREFERRED)
                .synonym("Axiom Syntax", TinkarTerm.ACCEPTABLE)
                .definition("Syntax defining description logic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, axiomSyntax.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .build();

        Concept expressAxiom = Concept.make("Express axiom syntax", uuidUtility.createUUID("Express axiom syntax"));
        starterData.concept(expressAxiom)
                .fullyQualifiedName(expressAxiom.description(), TinkarTerm.PREFERRED)
                .synonym("Express Axiom", TinkarTerm.ACCEPTABLE)
                .definition("Expressing description logic through syntax", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, expressAxiom.asUuidArray()[0].toString())
                .statedDefinition(List.of(axiomSyntax))
                .build();

        starterData.concept(TinkarTerm.ACCEPTABLE)
                .fullyQualifiedName("Acceptable (foundation metadata concept)", TinkarTerm.PREFERRED)
                .synonym("Acceptable (SOLOR)", TinkarTerm.PREFERRED)
                .definition("defines whethere a value is allowable for a given field", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ACCEPTABLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY))
                .build();

        starterData.concept(TinkarTerm.ACTIVE_STATE)
                .fullyQualifiedName("Active status (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Active", TinkarTerm.PREFERRED)
                .definition("the status of the concept (Active, Cancelled, Inactive, Primordial)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ACTIVE_STATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.STATUS_VALUE))
                .statedDefinition(List.of(TinkarTerm.STATUS_VALUE))
                .build();

        starterData.concept(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Allowed states for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Allowed states", TinkarTerm.PREFERRED)
                .definition("predefined list of values for STAMP coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.AND)
                .fullyQualifiedName("And (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("And", TinkarTerm.PREFERRED)
                .definition("an operator that typically is employed to combine two conditions", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AND.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.ANNOTATION_TYPE)
                .fullyQualifiedName("Annotation type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Annotation type", TinkarTerm.PREFERRED)
                .definition("metadata about program elements, and annotation types define the structure of these annotations", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANNOTATION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.COMMENT, TinkarTerm.KOMET_ISSUE), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        starterData.concept(TinkarTerm.ANONYMOUS_CONCEPT)
                .fullyQualifiedName("Anonynmous concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Anynonymous concept", TinkarTerm.PREFERRED)
                .definition("concepts or entities that do not have a speicfic, named identity, (defined on-the-fly without a dedicated name)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANONYMOUS_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCEPT_TYPE))
                .statedDefinition(List.of(TinkarTerm.CONCEPT_TYPE))
                .build();

        starterData.concept(TinkarTerm.ANY_COMPONENT)
                .fullyQualifiedName("Any component (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Any component", TinkarTerm.PREFERRED)
                .definition("a general-purpose container to represent any component with generic data structure. Modifyable based on the specific requirements and characteristics of the components.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANY_COMPONENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.ARRAY)
                .fullyQualifiedName("Array (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Array", TinkarTerm.PREFERRED)
                .definition("Linear data structure", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ARRAY.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.ARRAY_FIELD)
                .fullyQualifiedName("Array field (Solor)", TinkarTerm.PREFERRED)
                .synonym("Array field", TinkarTerm.PREFERRED)
                .definition("a lexical set of semantically related elements/items", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ARRAY_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE)
                .fullyQualifiedName("Author for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Author", TinkarTerm.PREFERRED)
                .definition("indidvidual or enitity who made a particular edit or reivsion in a document (authoring a specific location or point in the codebase where an edit was made)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.AUTHOR_FOR_VERSION)
                .fullyQualifiedName("Author for version (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Author", TinkarTerm.PREFERRED)
                .definition("indidvidual or enitity who made a speicfic set of changes or modifications to a codebase/terminology resulting in the creation of a new version or revision", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHOR_FOR_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Author for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Authors", TinkarTerm.PREFERRED)
                .definition("an individualy or an entity responsible for defining or updating the values associated with the STAMP coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.AXIOM_FOCUS)
                .fullyQualifiedName("Axiom focus (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Axiom focus", TinkarTerm.PREFERRED)
                .definition("a statement or proposition that is assumed to be true without requiring proof, it serves as a foundation principles on which a system or theory is built. Focus refers to the central point of attention or concentration on a speicfic concept/axioms", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AXIOM_FOCUS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .statedDefinition(List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .build();

        starterData.concept(TinkarTerm.AXIOM_ORIGIN)
                .fullyQualifiedName("Axiom origin (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Axiom origin", TinkarTerm.PREFERRED)
                .definition("the parent concept for the axiom?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AXIOM_ORIGIN.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.STATED_PREMISE_TYPE, TinkarTerm.INFERRED_PREMISE_TYPE), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_FIELD)
                .fullyQualifiedName("Boolean field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean field", TinkarTerm.PREFERRED)
                .definition("true (1) or false (0)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_LITERAL)
                .fullyQualifiedName("Boolean literal (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean literal", TinkarTerm.PREFERRED)
                .definition("TRUE, FALSE, UKNOWN", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_LITERAL.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LITERAL_VALUE))
                .statedDefinition(List.of(TinkarTerm.LITERAL_VALUE))
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_REFERENCE)
                .fullyQualifiedName("Boolean reference (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean reference", TinkarTerm.PREFERRED)
                .definition("Reference(a pointer) to a Boolean object", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_REFERENCE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.QUERY_CLAUSES))
                .statedDefinition(List.of(TinkarTerm.QUERY_CLAUSES))
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_SUBSTITUTION)
                .fullyQualifiedName("Boolean substitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean substitution", TinkarTerm.PREFERRED)
                .definition("the process of replacing or substituting boolean values or expression in a logical context", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.FIELD_SUBSTITUTION))
                .statedDefinition(List.of(TinkarTerm.FIELD_SUBSTITUTION))
                .build();

        starterData.concept(TinkarTerm.BYTE_ARRAY_FIELD)
                .fullyQualifiedName("Byte array field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Byte array field", TinkarTerm.PREFERRED)
                .definition("an arrary of bytes", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BYTE_ARRAY_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.CANCELED_STATE)
                .fullyQualifiedName("Canceled status (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Canceled", TinkarTerm.PREFERRED)
                .definition("the status of the concept (Active, Cancelled, Primordial)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CANCELED_STATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.STATUS_VALUE))
                .statedDefinition(List.of(TinkarTerm.STATUS_VALUE))
                .build();

        starterData.concept(TinkarTerm.CASE_INSENSITIVE_EVALUATION)
                .fullyQualifiedName("Case insensitive evaluation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Ignore case", TinkarTerm.PREFERRED)
                .definition("evaluates values regardless of the case", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_INSENSITIVE_EVALUATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(textComparison))
                .statedDefinition(List.of(textComparison))
                .build();

        starterData.concept(TinkarTerm.CASE_SENSITIVE_EVALUATION)
                .fullyQualifiedName("Case sensitive evaluation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Compare case", TinkarTerm.PREFERRED)
                .definition("evaluated based on the case", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_SENSITIVE_EVALUATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(textComparison))
                .statedDefinition(List.of(textComparison))
                .build();

        starterData.concept(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION)
                .fullyQualifiedName("Case significance concept nid for description (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Case significance", TinkarTerm.PREFERRED)
                .definition("a field label which captures the case significance for a given concept descrtiption", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.CHINESE_LANGUAGE)
                .fullyQualifiedName("Chinese language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Chinese language", TinkarTerm.PREFERRED)
                .definition("Chinese language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CHINESE_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.CHRONICLE_PROPERTIES)
                .fullyQualifiedName("Chronicle properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Chronicle properties", TinkarTerm.PREFERRED)
                .definition("attributes or characteristic associated with a historical record or an account of events (metadata, timestamps)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CHRONICLE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE, TinkarTerm.VERSION_LIST_FOR_CHRONICLE, TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE, TinkarTerm.UUID_LIST_FOR_COMPONENT), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.COMMENT)
                .fullyQualifiedName("Comment (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Comment", TinkarTerm.PREFERRED)
                .definition("A filed lable to capture free text information which may be necessary to add or change (concepts, relationships, semantics, etc)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMMENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ANNOTATION_TYPE))
                .statedDefinition(List.of(TinkarTerm.ANNOTATION_TYPE))
                .build();

        starterData.concept(TinkarTerm.COMPONENT_ID_LIST_FIELD)
                .fullyQualifiedName("Component Id list (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component Id list", TinkarTerm.PREFERRED)
                .definition("Id list of component", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_ID_LIST_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fullyQualifiedName("Component Id set field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component Id set", TinkarTerm.PREFERRED)
                .definition("Id set of component", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_ID_SET_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.COMPONENT_FIELD)
                .fullyQualifiedName("Component field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component field", TinkarTerm.PREFERRED)
                .definition("Component field", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.COMPONENT_FOR_SEMANTIC)
                .fullyQualifiedName("Component for semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component", TinkarTerm.PREFERRED)
                .definition("Component for semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_FOR_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.COMPONENT_SEMANTIC)
                .fullyQualifiedName("Component semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component Semantic", TinkarTerm.PREFERRED)
                .definition("Component semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .build();

        starterData.concept(TinkarTerm.COMPONENT_TYPE_FOCUS)
                .fullyQualifiedName("Component type focus (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Component type focus", TinkarTerm.PREFERRED)
                .definition("Focus type of component", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_TYPE_FOCUS.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.AXIOM_FOCUS, TinkarTerm.CONCEPT_FOCUS, TinkarTerm.DESCRIPTION_FOCUS), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_CONSTRAINTS)
                .fullyQualifiedName("Concept constraints(SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept constraints", TinkarTerm.PREFERRED)
                .definition("defined filters for a given concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_CONSTRAINTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ACTION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.ACTION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE)
                .fullyQualifiedName("Concept details tree table (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept details tree table", TinkarTerm.PREFERRED)
                .definition("Tree table with concept details", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_DETAILS_TREE_TABLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_FIELD)
                .fullyQualifiedName("Concept field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym(" Concept field", TinkarTerm.PREFERRED)
                .definition("field for the human readable description for the given concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_FOCUS)
                .fullyQualifiedName("Concept focus (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept focus", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_FOCUS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .statedDefinition(List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_REFERENCE)
                .fullyQualifiedName("Concept reference (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept reference", TinkarTerm.PREFERRED)
                .definition("a field to capture a refrence to validate concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_REFERENCE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_SEMANTIC)
                .fullyQualifiedName("Concept semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept semantic", TinkarTerm.PREFERRED)
                .definition("value to define a given semantic as a concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_SUBSTITUTION)
                .fullyQualifiedName("Concept subtitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept subsitution", TinkarTerm.PREFERRED)
                .definition("Substitution for concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.FIELD_SUBSTITUTION))
                .statedDefinition(List.of(TinkarTerm.FIELD_SUBSTITUTION))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_TO_FIND)
                .fullyQualifiedName("Concept to find (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept to find", TinkarTerm.PREFERRED)
                .definition("Find concept (if searching on Komet shows us the results 'details and futher information?)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_TO_FIND.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ACTION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.ACTION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_TYPE)
                .fullyQualifiedName("Concept type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept type", TinkarTerm.PREFERRED)
                .definition("a field that captures a defined concept label", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_TYPE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ANONYMOUS_CONCEPT, TinkarTerm.HEALTH_CONCEPT, TinkarTerm.PATH_CONCEPT, TinkarTerm.SEMANTIC_FIELD_CONCEPTS), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_VERSION)
                .fullyQualifiedName("Concept version (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Version", TinkarTerm.PREFERRED)
                .definition("a filed that captures the version of the terminology that it came from", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.CONCRETE_DOMAIN_OPERATOR)
                .fullyQualifiedName("Concrete domain operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concrete domain operator", TinkarTerm.PREFERRED)
                .definition("Domain operators for concepts", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCRETE_DOMAIN_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EQUAL_TO, TinkarTerm.GREATER_THAN, TinkarTerm.GREATER_THAN_OR_EQUAL_TO, TinkarTerm.LESS_THAN, TinkarTerm.LESS_THAN_OR_EQUAL_TO), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.CONDITIONAL_TRIGGERS)
                .fullyQualifiedName("Conditional triggers (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Conditional triggers", TinkarTerm.PREFERRED)
                .definition("Conditional triggers based on actions, reasoner", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONDITIONAL_TRIGGERS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ACTION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.ACTION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.CONNECTIVE_OPERATOR)
                .fullyQualifiedName("Conncective operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Connective operator", TinkarTerm.PREFERRED)
                .definition("a filed that captures what the operator is  (logical connective)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONNECTIVE_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.AND, TinkarTerm.DEFINITION_ROOT, TinkarTerm.DISJOINT_WITH, TinkarTerm.OR, TinkarTerm.IS_A, TinkarTerm.PART_OF, TinkarTerm.CONCEPT_REFERENCE), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.CORELATION_EXPRESSION)
                .fullyQualifiedName("Corelation expresson (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Corelation expression", TinkarTerm.PREFERRED)
                .definition("a value for Corelation properties", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CORELATION_EXPRESSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CORRELATION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.CORRELATION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.CORELATION_REFERENCE_EXPRESSION)
                .fullyQualifiedName("Corelation reference expresson (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Corelation reference expression", TinkarTerm.PREFERRED)
                .definition("a value for corelation", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CORELATION_REFERENCE_EXPRESSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CORRELATION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.CORRELATION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.CORRELATION_PROPERTIES)
                .fullyQualifiedName("Correlation properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Correlation properties", TinkarTerm.PREFERRED)
                .definition("characteristics or measures that describe the relationship between two or more variables", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CORRELATION_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CORELATION_EXPRESSION, TinkarTerm.CORELATION_REFERENCE_EXPRESSION), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE)
                .fullyQualifiedName("Creative Commons BY license (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Creative Commons BY license", TinkarTerm.PREFERRED)
                .definition("Creative Commons (CC) licenses are a set of public copyright licenses that enable the free distribution of an otherwise copyrighted work", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CREATIVE_COMMONS_BY_LICENSE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        starterData.concept(TinkarTerm.CZECH_DIALECT)
                .fullyQualifiedName("Czech dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Czech dialect", TinkarTerm.PREFERRED)
                .definition("Czech dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CZECH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .build();

        starterData.concept(TinkarTerm.CZECH_LANGUAGE)
                .fullyQualifiedName("Czech language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Czech language", TinkarTerm.PREFERRED)
                .definition("Czech Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CZECH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.DANISH_LANGUAGE)
                .fullyQualifiedName("Danish language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Danish language", TinkarTerm.PREFERRED)
                .definition("Danish Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DANISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE)
                .fullyQualifiedName("Default module for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Default module", TinkarTerm.PREFERRED)
                .definition("a value for coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.DEFINITION_DESCRIPTION_TYPE)
                .fullyQualifiedName("Definition description type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Definition", TinkarTerm.PREFERRED)
                .definition("a value for description type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFINITION_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.DEFINITION_ROOT)
                .fullyQualifiedName("Definition root (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Definition root", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFINITION_ROOT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION)
                .fullyQualifiedName("Description (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description", TinkarTerm.PREFERRED)
                .definition("A concept used as a placeholder in the computabel columns configuration of maps -> this is a komet defintion - (a value for maping display field which captures human readable name for a concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY, TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE, TinkarTerm.DESCRIPTION_TYPE, TinkarTerm.TEXT_FOR_DESCRIPTION, TinkarTerm.DESCRIPTION_LOGIC_PROFILE), List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                .fullyQualifiedName("Description acceptability (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description acceptability", TinkarTerm.PREFERRED)
                .definition("defining metadata to the nth degree- whether a given human readable term for a concept is permissible", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_ACCEPTABILITY.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ACCEPTABLE, TinkarTerm.PREFERRED), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CASE_SENSITIVE)
                .fullyQualifiedName("Description case sensitive (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Case sensitive", TinkarTerm.PREFERRED)
                .definition("a field value for case significance for a description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CASE_SENSITIVE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE)
                .fullyQualifiedName("Description case significance (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description case significance", TinkarTerm.PREFERRED)
                .definition("a field label which is looking for a value to define case significance for a description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DESCRIPTION_CASE_SENSITIVE, TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE, TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE, TinkarTerm.NOT_APPLICABLE), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CORE_TYPE)
                .fullyQualifiedName("Description core type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description core type", TinkarTerm.PREFERRED)
                .definition("Used to mark non-snomed descriptions as one of the core snomed types", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CORE_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_DIALECT_PAIR)
                .fullyQualifiedName("Description dialect pair (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description dialect pair", TinkarTerm.PREFERRED)
                .definition("Description dialect pair - linking together dialects with language descriptions", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_DIALECT_PAIR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR, TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR), List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_FOCUS)
                .fullyQualifiedName("Description focus (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description focus ", TinkarTerm.PREFERRED)
                .definition("Description focus", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_FOCUS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .statedDefinition(List.of(TinkarTerm.COMPONENT_TYPE_FOCUS))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR)
                .fullyQualifiedName("Description for dialect/description pair (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description for dialect/description pair", TinkarTerm.PREFERRED)
                .definition("linking together dialects with language descriptions", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE)
                .fullyQualifiedName("Description initial character case sensitive (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Initial characted case insensitive", TinkarTerm.PREFERRED)
                .definition("value which designates initial character as sensitive for a given description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName("Description logic profile for logic coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logic profile", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE)
                .fullyQualifiedName("Decription not case sensitive (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Case insensitive", TinkarTerm.PREFERRED)
                .definition("value which designate character as not sensitive for a given description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fullyQualifiedName("Description semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description semantic", TinkarTerm.PREFERRED)
                .definition("value for semantic type which designates it as a description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE)
                .fullyQualifiedName("Description type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description type", TinkarTerm.PREFERRED)
                .definition("specifying what type of description it is i.e. is it fully qualified or regular and etc.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DEFINITION_DESCRIPTION_TYPE, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE, TinkarTerm.DESCRIPTION_CORE_TYPE, TinkarTerm.EXTENDED_DESCRIPTION_TYPE, TinkarTerm.EXTENDED_RELATIONSHIP_TYPE, TinkarTerm.INVERSE_NAME), List.of(TinkarTerm.DESCRIPTION))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION)
                .fullyQualifiedName("Description type for description (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Decription type", TinkarTerm.PREFERRED)
                .definition("linking for each description -> what type it is", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName("Description type preference list for language coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Type order", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES)
                .fullyQualifiedName("Description version properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description version properties", TinkarTerm.PREFERRED)
                .definition("combination of terms that might be used in a specific context or domain", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_VERSION_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION, TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION, TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION, TinkarTerm.DESCRIPTION_DIALECT_PAIR), List.of(TinkarTerm.VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE)
                .fullyQualifiedName("Decription-logic profile (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description-logic profile", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_LOGIC_PROFILE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EL_PLUS_PLUS_PROFILE), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTUM)
                .fullyQualifiedName("Descriptum (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Descriptum", TinkarTerm.PREFERRED)
                .definition("refers to something that has been described or the result of a description - the item being described", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTUM.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PURPOSE))
                .statedDefinition(List.of(TinkarTerm.PURPOSE))
                .build();

        starterData.concept(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE)
                .fullyQualifiedName("Destination module for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Destination module", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.DEVELOPMENT_MODULE)
                .fullyQualifiedName("Development module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Development module", TinkarTerm.PREFERRED)
                .definition("predefines or standard module within a systsem or application that is specifically desgined to support the development phase of a project", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEVELOPMENT_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODULE))
                .statedDefinition(List.of(TinkarTerm.MODULE))
                .build();

        starterData.concept(TinkarTerm.DEVELOPMENT_PATH)
                .fullyQualifiedName("Development path (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Development path", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEVELOPMENT_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH))
                .statedDefinition(List.of(TinkarTerm.PATH))
                .pathMembership()
                .build();

        starterData.concept(TinkarTerm.DIGRAPH_FIELD)
                .fullyQualifiedName("DiGraph field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Instant/ DiGraph", TinkarTerm.PREFERRED)
                .definition("Instant field - a directed graph is a collection of nodes (vertices) connected by edges, where each edge has a direction - a value which desribes a semantic field type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIGRAPH_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.DITREE_FIELD)
                .fullyQualifiedName("DiTree field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("DiTree", TinkarTerm.PREFERRED)
                .definition("a value which describes a semantic field type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DITREE_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR)
                .fullyQualifiedName("Dialect for dialect/description pair (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Dialect for dialect/description pair", TinkarTerm.PREFERRED)
                .definition("specific dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_DIALECT_PAIR))
                .build();

        starterData.concept(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName("Digraph for logic coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Digraph", TinkarTerm.PREFERRED)
                .definition("a value which describes a immutable coordinate property", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.DIRECTED_GRAPH)
                .fullyQualifiedName("Directed graph (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("NavigationCoordinate/Directed graph", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIRECTED_GRAPH.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EL_PLUS_PLUS_DIGRAPH), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.DISJOINT_WITH)
                .fullyQualifiedName("Disjjoint with (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Disjoint with", TinkarTerm.PREFERRED)
                .definition("an operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DISJOINT_WITH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.DISPLAY_FIELDS)
                .fullyQualifiedName("Display Fields (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Display fields", TinkarTerm.PREFERRED)
                .definition("Stores the mapping fields in an array corresponding to an ordered displayable row", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DISPLAY_FIELDS.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.SEMANTIC_FIELD_TYPE, TinkarTerm.ARRAY_FIELD, TinkarTerm.BOOLEAN_FIELD, TinkarTerm.BYTE_ARRAY_FIELD, TinkarTerm.COMPONENT_ID_LIST_FIELD, TinkarTerm.COMPONENT_ID_SET_FIELD, TinkarTerm.COMPONENT_FIELD, TinkarTerm.CONCEPT_FIELD, TinkarTerm.DIGRAPH_FIELD, TinkarTerm.DITREE_FIELD, TinkarTerm.DOUBLE_FIELD, TinkarTerm.FLOAT_FIELD, TinkarTerm.LOGICAL_EXPRESSION_FIELD, TinkarTerm.UUID_FIELD, TinkarTerm.VERTEX_FIELD), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.DOUBLE_FIELD)
                .fullyQualifiedName("Double field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Double field", TinkarTerm.PREFERRED)
                .definition("a data value (type of structure for data)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DOUBLE_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.DUTCH_LANGUAGE)
                .fullyQualifiedName("Dutch language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Dutch language", TinkarTerm.PREFERRED)
                .definition("Dutch language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DUTCH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.EL_PROFILE_SET_OPERATOR)
                .fullyQualifiedName("El profile set operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL profile set operator", TinkarTerm.PREFERRED)
                .definition("EL profile set operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PROFILE_SET_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.NECESSARY_SET, TinkarTerm.SUFFICIENT_SET), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION)
                .fullyQualifiedName("El++ Inferred Concept Definition (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL++ Inferred Concept Definition", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LOGICAL_DEFINITION))
                .statedDefinition(List.of(TinkarTerm.LOGICAL_DEFINITION))
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)
                .fullyQualifiedName("EL++ Inferred terminological axioms (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL++ Inferred terminological axioms", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LOGICAL_DEFINITION))
                .statedDefinition(List.of(TinkarTerm.LOGICAL_DEFINITION))
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION)
                .fullyQualifiedName("EL++ Stated Concept Definition (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL++ Stated Concept Definition (SOLOR)", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LOGICAL_DEFINITION))
                .statedDefinition(List.of(TinkarTerm.LOGICAL_DEFINITION))
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)
                .fullyQualifiedName("EL++ Stated terminological axioms (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL++ Stated terminological axioms", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LOGICAL_DEFINITION))
                .statedDefinition(List.of(TinkarTerm.LOGICAL_DEFINITION))
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_DIGRAPH)
                .fullyQualifiedName("EL++ digraph (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL++ digraph", TinkarTerm.PREFERRED)
                .definition("The directed graph that results from classifying a set of EL++ axioms", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_DIGRAPH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DIRECTED_GRAPH))
                .statedDefinition(List.of(TinkarTerm.DIRECTED_GRAPH))
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_PROFILE)
                .fullyQualifiedName("EL++ logic profile (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("EL ++ logic profile", TinkarTerm.PREFERRED)
                .definition("EL ++ profile", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_PROFILE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_LOGIC_PROFILE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_LOGIC_PROFILE))
                .build();

        starterData.concept(TinkarTerm.ENGLISH_LANGUAGE)
                .fullyQualifiedName("English Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("English language", TinkarTerm.PREFERRED)
                .definition("English Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ENGLISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.EQUAL_TO)
                .fullyQualifiedName("Equal to (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Equal to", TinkarTerm.PREFERRED)
                .definition("an operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EQUAL_TO.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.EXACT)
                .fullyQualifiedName("Exact (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Exact", TinkarTerm.PREFERRED)
                .definition("Source and target are semantic or exact lexical match", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXACT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.GROUPING))
                .statedDefinition(List.of(TinkarTerm.GROUPING))
                .build();

        starterData.concept(TinkarTerm.EXTENDED_DESCRIPTION_TYPE)
                .fullyQualifiedName("Extended description type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Extended description type", TinkarTerm.PREFERRED)
                .definition("Used to store non-snomed description types when other terminologies are imported", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXTENDED_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE)
                .fullyQualifiedName("Extended relationship type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Extended relationship type", TinkarTerm.PREFERRED)
                .definition("Used to store non-snomed relationship types when other terminologies are imported- especially when a relationship is mapped onto a snomed relationship type (such as isa)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXTENDED_RELATIONSHIP_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.FLOAT_FIELD)
                .fullyQualifiedName("Float field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Float field", TinkarTerm.PREFERRED)
                .definition("floating point number supporting arithmetic operations", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.FLOAT_LITERAL)
                .fullyQualifiedName("Float literal (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Float literal", TinkarTerm.PREFERRED)
                .definition("Numbers with decimal point or an exponential part", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_LITERAL.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LITERAL_VALUE))
                .statedDefinition(List.of(TinkarTerm.LITERAL_VALUE))
                .build();

        starterData.concept(TinkarTerm.FLOAT_SUBSTITUTION)
                .fullyQualifiedName("Float substitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Float substitution", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.FIELD_SUBSTITUTION))
                .statedDefinition(List.of(TinkarTerm.FIELD_SUBSTITUTION))
                .build();

        starterData.concept(TinkarTerm.FRENCH_DIALECT)
                .fullyQualifiedName("French dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("French dialect", TinkarTerm.PREFERRED)
                .definition("French dialect ", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FRENCH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .build();

        starterData.concept(TinkarTerm.FRENCH_LANGUAGE)
                .fullyQualifiedName("French Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("French language", TinkarTerm.PREFERRED)
                .definition("French Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FRENCH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE)
                .fullyQualifiedName("Fully qualified name description type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Fully quailified name", TinkarTerm.PREFERRED)
                .definition(" it is the unambigious, human readable term for a concept as a value in the description type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.GB_ENGLISH_DIALECT)
                .fullyQualifiedName("Great Britian English dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("GB English dialect / GB English", TinkarTerm.PREFERRED)
                .definition("Great Britain: English Langauge reference set", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GB_ENGLISH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(englishDialect))
                .statedDefinition(List.of(englishDialect))
                .build();

        starterData.concept(TinkarTerm.GERMAN_LANGUAGE)
                .fullyQualifiedName("German Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("German language", TinkarTerm.PREFERRED)
                .definition("German Language ", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GERMAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.GREATER_THAN)
                .fullyQualifiedName("Greater than (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Greater than", TinkarTerm.PREFERRED)
                .definition("an operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GREATER_THAN.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.GREATER_THAN_OR_EQUAL_TO)
                .fullyQualifiedName("Greater than or equal to (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Greater than or equal to", TinkarTerm.PREFERRED)
                .definition("an operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GREATER_THAN_OR_EQUAL_TO.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.GROUPING)
                .fullyQualifiedName("Grouping (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Grouping", TinkarTerm.PREFERRED)
                .definition("The grouping attached to the mapping", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GROUPING.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.HEALTH_CONCEPT)
                .fullyQualifiedName("Health concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Health concept", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.HEALTH_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCEPT_TYPE))
                .statedDefinition(List.of(TinkarTerm.CONCEPT_TYPE))
                .build();

        starterData.concept(TinkarTerm.IDENTIFIER_SOURCE)
                .fullyQualifiedName("Identifier source (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Identifier source", TinkarTerm.PREFERRED)
                .definition("A parent concept and membership assemblage used to group identifiers", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IDENTIFIER_SOURCE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.INACTIVE_STATE)
                .fullyQualifiedName("Inactive status (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Inactive", TinkarTerm.PREFERRED)
                .definition("the status of the concept (Active, Cancelled, Inactive, Primordial)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INACTIVE_STATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.STATUS_VALUE))
                .statedDefinition(List.of(TinkarTerm.STATUS_VALUE))
                .build();

        starterData.concept(TinkarTerm.INFERRED_PREMISE_TYPE)
                .fullyQualifiedName("Inferred premise type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Inferred relationship / Inferred", TinkarTerm.PREFERRED)
                .definition("the axiom view following the application of the reasoner", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INFERRED_PREMISE_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.AXIOM_ORIGIN))
                .statedDefinition(List.of(TinkarTerm.AXIOM_ORIGIN))
                .build();

        starterData.concept(TinkarTerm.INSTANT_LITERAL)
                .fullyQualifiedName("Instant literal (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Instant literal", TinkarTerm.PREFERRED)
                .definition("may refer to a speicfic point in time which is often represented by a date or time value", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INSTANT_LITERAL.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LITERAL_VALUE))
                .statedDefinition(List.of(TinkarTerm.LITERAL_VALUE))
                .build();

        starterData.concept(TinkarTerm.INSTANT_SUBSTITUTION)
                .fullyQualifiedName("Instant substitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Instant substitution", TinkarTerm.PREFERRED)
                .definition("subtitution of instant literal?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INSTANT_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.FIELD_SUBSTITUTION))
                .statedDefinition(List.of(TinkarTerm.FIELD_SUBSTITUTION))
                .build();

        starterData.concept(TinkarTerm.INVERSE_NAME)
                .fullyQualifiedName("Inverse name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Inverse name", TinkarTerm.PREFERRED)
                .definition("This is the extended desrciption type that maye be attached to a description within a concept that defines as Association refex to signify that the referenced description  is the inverse of the association name", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INVERSE_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.INVERSE_TREE_LIST)
                .fullyQualifiedName("Inverse tree list (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Inverse tree list", TinkarTerm.PREFERRED)
                .definition("Inverse tree list", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INVERSE_TREE_LIST.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.TREE_AMALGAM_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.TREE_AMALGAM_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.IRISH_DIALECT)
                .fullyQualifiedName("Irish dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Irish dialect", TinkarTerm.PREFERRED)
                .definition("Irish dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IRISH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .build();

        starterData.concept(TinkarTerm.IRISH_LANGUAGE)
                .fullyQualifiedName("Irish language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Irish language", TinkarTerm.PREFERRED)
                .definition("Irish language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IRISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.IS_A)
                .fullyQualifiedName("Is-a (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Is a", TinkarTerm.PREFERRED)
                .definition("designates the parent child relationship", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.IS_A_INFERRED_NAVIGATION)
                .fullyQualifiedName("Is-a inferred navigation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Is-a inferred navigation (SOLOR)", TinkarTerm.PREFERRED)
                .definition("designates the parent child relationship following the application of the reasoner", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A_INFERRED_NAVIGATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.NAVIGATION_PATTERN))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION_PATTERN))
                .build();

        starterData.concept(TinkarTerm.IS_A_STATED_NAVIGATION)
                .fullyQualifiedName("Is-a stated navigation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("is-a stated navigation", TinkarTerm.PREFERRED)
                .definition("designates the parent child relationship as authored", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A_STATED_NAVIGATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.NAVIGATION_PATTERN))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION_PATTERN))
                .build();

        starterData.concept(TinkarTerm.ITALIAN_LANGUAGE)
                .fullyQualifiedName("Italian Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Italian language", TinkarTerm.PREFERRED)
                .definition("Italian language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ITALIAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.KOMET_MODULE)
                .fullyQualifiedName("KOMET module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("KOMET module", TinkarTerm.PREFERRED)
                .definition("Komet specific values?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODULE))
                .statedDefinition(List.of(TinkarTerm.MODULE))
                .build();

        starterData.concept(TinkarTerm.KOMET_USER)
                .fullyQualifiedName("KOMET user (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("KOMET user", TinkarTerm.PREFERRED)
                .definition("Authorized to author, edit and/or view in Komet", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_USER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        starterData.concept(TinkarTerm.KOMET_USER_LIST)
                .fullyQualifiedName("KOMET user list (SOLOR", TinkarTerm.PREFERRED)
                .synonym("KOMET user list", TinkarTerm.PREFERRED)
                .definition("inventory of authorized komet users", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_USER_LIST.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        starterData.concept(TinkarTerm.KOMET_ISSUE)
                .fullyQualifiedName("Komet issue (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Komet issue", TinkarTerm.PREFERRED)
                .definition("komet being the 'annotation type' - specified type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_ISSUE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ANNOTATION_TYPE))
                .statedDefinition(List.of(TinkarTerm.ANNOTATION_TYPE))
                .build();

        starterData.concept(TinkarTerm.KOREAN_DIALECT)
                .fullyQualifiedName("Korean dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Korean dialect", TinkarTerm.PREFERRED)
                .definition("Korean dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOREAN_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.STANDARD_KOREAN_DIALECT), List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .build();

        starterData.concept(TinkarTerm.KOREAN_LANGUAGE)
                .fullyQualifiedName("Korean Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Korean language", TinkarTerm.PREFERRED)
                .definition("Korean language ", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOREAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.LANGUAGE)
                .fullyQualifiedName("Language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language", TinkarTerm.PREFERRED)
                .definition("field which captures the defined language for a komet description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CHINESE_LANGUAGE, TinkarTerm.CZECH_LANGUAGE, TinkarTerm.DANISH_LANGUAGE, TinkarTerm.DUTCH_LANGUAGE, TinkarTerm.ENGLISH_LANGUAGE, TinkarTerm.FRENCH_LANGUAGE, TinkarTerm.GERMAN_LANGUAGE, TinkarTerm.IRISH_LANGUAGE, TinkarTerm.ITALIAN_LANGUAGE, TinkarTerm.KOREAN_LANGUAGE, TinkarTerm.LITHUANIAN_LANGUAGE, TinkarTerm.POLISH_LANGUAGE, TinkarTerm.RUSSIAN_LANGUAGE, TinkarTerm.SPANISH_LANGUAGE, TinkarTerm.SWEDISH_LANGUAGE), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION)
                .fullyQualifiedName("Language concept nid for description (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language for description", TinkarTerm.PREFERRED)
                .definition("captures the language code for a descriptiom", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_NAME)
                .fullyQualifiedName("Language coordinate name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language coordinate name", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_COORDINATE_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES)
                .fullyQualifiedName("Language coordinate properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Langauge coordinate properties", TinkarTerm.PREFERRED)
                .definition("Spatial representation of language, attributes or language coordinates, programming language metadata?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.LANGUAGE_COORDINATE_NAME), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName("Lanuage nid for language coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language nid", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName("Language specification for language coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Language", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LESS_THAN)
                .fullyQualifiedName("Less than (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Less than", TinkarTerm.PREFERRED)
                .definition("an operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LESS_THAN.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.LESS_THAN_OR_EQUAL_TO)
                .fullyQualifiedName("Less than or equal to (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Less than or eual to", TinkarTerm.PREFERRED)
                .definition("an operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LESS_THAN_OR_EQUAL_TO.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONCRETE_DOMAIN_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.LITHUANIAN_LANGUAGE)
                .fullyQualifiedName("Lithuanian langauge (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Lithuanian Language", TinkarTerm.PREFERRED)
                .definition("Lithuanian Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LITHUANIAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.LOGIC_COORDINATE_NAME)
                .fullyQualifiedName("Logic coordinate name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logic coordinate name", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGIC_COORDINATE_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LOGIC_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.LOGIC_COORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LOGIC_COORDINATE_PROPERTIES)
                .fullyQualifiedName("Logic coordinate properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logic coordinate properites", TinkarTerm.PREFERRED)
                .definition("Structural characteristics of logical elements, Attributes of Logical coordinates, Mathematical Representation of logical relationships ?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGIC_COORDINATE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.LOGIC_COORDINATE_NAME), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LOGICAL_DEFINITION)
                .fullyQualifiedName("Logical Definition (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logical Definition", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION, TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS), List.of(TinkarTerm.PURPOSE))
                .statedDefinition(List.of(TinkarTerm.PURPOSE))
                .build();

        starterData.concept(TinkarTerm.LOGICAL_EXPRESSION_FIELD)
                .fullyQualifiedName("Logical expression field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logical expression field", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_EXPRESSION_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC)
                .fullyQualifiedName("Logical expression semantic  (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logical expression semantic", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .build();

        starterData.concept(TinkarTerm.LOGICALLY_EQUIVALENT_TO)
                .fullyQualifiedName("Logically eqivalent to (Solor)", TinkarTerm.PREFERRED)
                .synonym("Logically equivalent to", TinkarTerm.PREFERRED)
                .definition("an operator for the reasoner to determine the equivalence", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICALLY_EQUIVALENT_TO.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.TAXONOMY_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.TAXONOMY_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.MASTER_PATH)
                .fullyQualifiedName("Master path (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Master path", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MASTER_PATH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MASTER_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH))
                .statedDefinition(List.of(TinkarTerm.PATH))
                .pathMembership()
                .build();

        starterData.concept(TinkarTerm.MEANING)
                .fullyQualifiedName("Meaning  (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Meaning", TinkarTerm.PREFERRED)
                .definition("the interpretation and explanation which provides a definition for a komet concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MEANING.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.SEMANTIC_TYPE, TinkarTerm.DISPLAY_FIELDS, TinkarTerm.FIELD_SUBSTITUTION, TinkarTerm.LITERAL_VALUE, TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES, TinkarTerm.PRESENTATION_UNIT_DIFFERENT, TinkarTerm.EL_PROFILE_SET_OPERATOR, TinkarTerm.CONNECTIVE_OPERATOR, TinkarTerm.CONCRETE_DOMAIN_OPERATOR, textComparison, TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR, TinkarTerm.TAXONOMY_OPERATOR, TinkarTerm.QUERY_CLAUSES, TinkarTerm.RELATIONSHIP_DESTINATION, TinkarTerm.RELATIONSHIP_ORIGIN), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .fullyQualifiedName("Membership semnatic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Membership semantic", TinkarTerm.PREFERRED)
                .definition("Membership semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MEMBERSHIP_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_TYPE))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_TYPE))
                .build();

        starterData.concept(TinkarTerm.MODEL_CONCEPT)
                .fullyQualifiedName("Model concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Model concept", TinkarTerm.PREFERRED)
                .definition("abstract conceptualizations to concrete, quanttitative models used for simulation or analysis", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODEL_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE, TinkarTerm.CONCEPT_TYPE, TinkarTerm.CONCEPT_VERSION, TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT, TinkarTerm.AXIOM_ORIGIN, TinkarTerm.COMPONENT_TYPE_FOCUS, TinkarTerm.DIRECTED_GRAPH, TinkarTerm.IDENTIFIER_SOURCE, TinkarTerm.LANGUAGE, TinkarTerm.DIALECT_ASSEMBLAGE, TinkarTerm.PURPOSE), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        starterData.concept(TinkarTerm.MODULE)
                .fullyQualifiedName("Module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module", TinkarTerm.PREFERRED)
                .definition("Module (Core metadata concept)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DEVELOPMENT_MODULE, TinkarTerm.SANDBOX_MODULE, TinkarTerm.KOMET_MODULE, TinkarTerm.PRIMORDIAL_MODULE, TinkarTerm.USERS_MODULE), List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Module exclusion set for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module exclusions", TinkarTerm.PREFERRED)
                .definition("Module exclusion set for stamp coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.MODULE_FOR_USER)
                .fullyQualifiedName("Module for user (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module for user", TinkarTerm.PREFERRED)
                .definition("User preference for Module?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_FOR_USER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        starterData.concept(TinkarTerm.MODULE_FOR_VERSION)
                .fullyQualifiedName("Module for version (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module", TinkarTerm.PREFERRED)
                .definition("Module Version", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_FOR_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE)
                .fullyQualifiedName("Module options for edit coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module options", TinkarTerm.PREFERRED)
                .definition("Coordinate edit options for Module", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName("Module preference list for language coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module nids", TinkarTerm.PREFERRED)
                .definition("Module preference list for language coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Module preference list for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module Preference list", TinkarTerm.PREFERRED)
                .definition("Module preference list for stamp coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Module preference order for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Module order", TinkarTerm.PREFERRED)
                .definition("Module preference order for stamp coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.MODULES_FOR_STAMP_COORDINATE)
                .fullyQualifiedName("Modules for stamp coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Modules", TinkarTerm.PREFERRED)
                .definition("Stamp coordinate modules", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.NAVIGATION)
                .fullyQualifiedName("Navigation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Navigation", TinkarTerm.PREFERRED)
                .definition("Navigation", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.NAVIGATION_CONCEPT_SET, TinkarTerm.NAVIGATION_VERTEX, TinkarTerm.NAVIGATION_PATTERN), List.of(TinkarTerm.PURPOSE))
                .statedDefinition(List.of(TinkarTerm.PURPOSE))
                .build();

        starterData.concept(TinkarTerm.NAVIGATION_CONCEPT_SET)
                .fullyQualifiedName("Navigation concept set (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Navigation set", TinkarTerm.PREFERRED)
                .definition("Navigating sets of concepts?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION_CONCEPT_SET.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.NAVIGATION))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION))
                .build();

        starterData.concept(TinkarTerm.NAVIGATION_PATTERN)
                .fullyQualifiedName("Navigation pattern (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Navigation pattern", TinkarTerm.PREFERRED)
                .definition("Navigation pattern (Inferred/Stated)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION_PATTERN.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.IS_A_INFERRED_NAVIGATION, TinkarTerm.IS_A_STATED_NAVIGATION), List.of(TinkarTerm.NAVIGATION))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION))
                .build();

        starterData.concept(TinkarTerm.NAVIGATION_VERTEX)
                .fullyQualifiedName("Navigation vertex (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Navigation vertex", TinkarTerm.PREFERRED)
                .definition("Navigation vertex", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION_VERTEX.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.NAVIGATION))
                .statedDefinition(List.of(TinkarTerm.NAVIGATION))
                .build();

        starterData.concept(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION)
                .fullyQualifiedName("Necessary but not sufficicent concept definition (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Necessary but not sufficient concept definition", TinkarTerm.PREFERRED)
                .definition("Not sufficicently defined by necessary conditions defintion status", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.NECESSARY_SET)
                .fullyQualifiedName("Necessary set (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Necessary set", TinkarTerm.PREFERRED)
                .definition("Required set", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NECESSARY_SET.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.EL_PROFILE_SET_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.EL_PROFILE_SET_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.NOT_APPLICABLE)
                .fullyQualifiedName("Not Applicable (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Not applicable", TinkarTerm.PREFERRED)
                .definition("Not available", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NOT_APPLICABLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE))
                .build();

        starterData.concept(TinkarTerm.OBJECT)
                .fullyQualifiedName("Object (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Object", TinkarTerm.PREFERRED)
                .definition("an encapsulation of data together with procedures", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.OBJECT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.STATUS_VALUE, TinkarTerm.DESCRIPTION, TinkarTerm.NID, TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANY_COMPONENT, TinkarTerm.UNINITIALIZED_COMPONENT, TinkarTerm.SANDBOX_COMPONENT, TinkarTerm.MODULE, TinkarTerm.PATH, TinkarTerm.PATH_ORIGINS_PATTERN, TinkarTerm.PATHS_PATTERN, TinkarTerm.OBJECT_PROPERTIES), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        starterData.concept(TinkarTerm.OBJECT_PROPERTIES)
                .fullyQualifiedName("Object Properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Object properties", TinkarTerm.PREFERRED)
                .definition("objects are instances of classes, the properties describe the data or attributes that an object can have", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.OBJECT_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ACTION_PROPERTIES, TinkarTerm.CHRONICLE_PROPERTIES, TinkarTerm.VERSION_PROPERTIES, TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES, TinkarTerm.LANGUAGE_COORDINATE_PROPERTIES, TinkarTerm.LOGIC_COORDINATE_PROPERTIES, TinkarTerm.PATH_COORDINATE_PROPERTIES, TinkarTerm.SEMANTIC_PROPERTIES, TinkarTerm.TREE_AMALGAM_PROPERTIES, TinkarTerm.CORRELATION_PROPERTIES), List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.OR)
                .fullyQualifiedName("Or (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Or", TinkarTerm.PREFERRED)
                .definition("Operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.OR.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS)
                .fullyQualifiedName("Order for axiom attachments (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Axiom attachment order(SOLOR)", TinkarTerm.PREFERRED)
                .definition("Order in which axioms are attached", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS)
                .fullyQualifiedName("Order for concept attachments  (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Concept attachment order", TinkarTerm.PREFERRED)
                .definition("Order in which concepts are attached", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS)
                .fullyQualifiedName("Order for description attachments (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description attachment order", TinkarTerm.PREFERRED)
                .definition("Order in which descriptions are attached", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        starterData.concept(TinkarTerm.PART_OF)
                .fullyQualifiedName("Part of (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Part of", TinkarTerm.PREFERRED)
                .definition("Part of an attribute", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PART_OF.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.CONNECTIVE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.PARTIAL)
                .fullyQualifiedName("Partial (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Partial", TinkarTerm.PREFERRED)
                .definition("Exists in/ Inclusion of ?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PARTIAL.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.GROUPING))
                .statedDefinition(List.of(TinkarTerm.GROUPING))
                .build();

        starterData.concept(TinkarTerm.PATH)
                .fullyQualifiedName("Path (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path", TinkarTerm.PREFERRED)
                .definition("Type of path", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DEVELOPMENT_PATH, TinkarTerm.MASTER_PATH, TinkarTerm.PRIMORDIAL_PATH, TinkarTerm.SANDBOX_PATH), List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.PATH_CONCEPT)
                .fullyQualifiedName("Path concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path concept", TinkarTerm.PREFERRED)
                .definition("Path concept", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCEPT_TYPE))
                .statedDefinition(List.of(TinkarTerm.CONCEPT_TYPE))
                .build();

        starterData.concept(TinkarTerm.PATH_COORDINATE_NAME)
                .fullyQualifiedName("Path coordinate name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path coordinate name", TinkarTerm.PREFERRED)
                .definition("Path coordinate name", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_COORDINATE_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PATH_COORDINATE_PROPERTIES)
                .fullyQualifiedName("Path coordinate properites (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path coordinate properties", TinkarTerm.PREFERRED)
                .definition("character or attribute of coordinates refering to a series of connected points, that form a shape or trajectory", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_COORDINATE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.PATH_COORDINATE_NAME, TinkarTerm.PATH_ORIGINS), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_PATH_COORDINATE)
                .fullyQualifiedName("Path for path cooridnate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path", TinkarTerm.PREFERRED)
                .definition("Path for path coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_PATH_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_USER)
                .fullyQualifiedName("Path for user (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path for user", TinkarTerm.PREFERRED)
                .definition("Path for user", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_USER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.USER))
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_VERSION)
                .fullyQualifiedName("Path for version", TinkarTerm.PREFERRED)
                .synonym("Path", TinkarTerm.PREFERRED)
                .definition("Version path", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE)
                .fullyQualifiedName("Path options for edit cordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path options", TinkarTerm.PREFERRED)
                .definition("Path options for edit coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PATH_ORIGINS)
                .fullyQualifiedName("Path origins (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path origins", TinkarTerm.PREFERRED)
                .definition("Path origins", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_ORIGINS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PATH_ORIGINS_PATTERN)
                .fullyQualifiedName("Path origins pattern (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path origins pattern", TinkarTerm.PREFERRED)
                .definition("Pattern of path origins", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_ORIGINS_PATTERN.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH)
                .fullyQualifiedName("Path origins for stamp path (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Path origins", TinkarTerm.PREFERRED)
                .definition("Path origins for stamp path", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PROMOTION_PATH_FOR_EDIT_CORDINATE)
                .fullyQualifiedName("Promotion Path for Edit Coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Promotion Path for Edit Coordinate", TinkarTerm.PREFERRED)
                .definition("Promotion Path for Edit Coordinate", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PROMOTION_PATH_FOR_EDIT_CORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.PATH_COORDINATE_PROPERTIES))
                .build();


        starterData.concept(TinkarTerm.PHENOMENON)
                .fullyQualifiedName("Phenomenon (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Phenomenon", TinkarTerm.PREFERRED)
                .definition("Phenomenon", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PHENOMENON.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.UNCATEGORIZED_PHENOMENON), List.of(TinkarTerm.HEALTH_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.HEALTH_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.POLISH_DIALECT)
                .fullyQualifiedName("Polish dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Polish dialect", TinkarTerm.PREFERRED)
                .definition("Polish Dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.POLISH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .build();

        starterData.concept(TinkarTerm.POLISH_LANGUAGE)
                .fullyQualifiedName("Polish Language (Language)", TinkarTerm.PREFERRED)
                .synonym("Polish language", TinkarTerm.PREFERRED)
                .definition("Polish Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.POLISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.PREFERRED)
                .fullyQualifiedName("Preffered (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Preffered", TinkarTerm.PREFERRED)
                .definition("Preffered( Foundation metadata concept)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PREFERRED.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_ACCEPTABILITY))
                .build();

        starterData.concept(TinkarTerm.PRESENTATION_UNIT_DIFFERENT)
                .fullyQualifiedName("Presentation unit different (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Presentation unit different", TinkarTerm.PREFERRED)
                .definition("Unit difference", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRESENTATION_UNIT_DIFFERENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE)
                .fullyQualifiedName("Primodridal UUID for chronicle (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Primordial UUID", TinkarTerm.PREFERRED)
                .definition("Primordial UUID", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CHRONICLE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.CHRONICLE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_MODULE)
                .fullyQualifiedName("Primordial module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Primordial module", TinkarTerm.PREFERRED)
                .definition("Primordial module", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODULE))
                .statedDefinition(List.of(TinkarTerm.MODULE))
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_PATH)
                .fullyQualifiedName("Primordial path (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Primordial path", TinkarTerm.PREFERRED)
                .definition("Primodial path", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH))
                .statedDefinition(List.of(TinkarTerm.PATH))
                .pathMembership()
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_STATE)
                .fullyQualifiedName("Primordial status (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Primordial", TinkarTerm.PREFERRED)
                .definition("Primordial status", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_STATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.STATUS_VALUE))
                .statedDefinition(List.of(TinkarTerm.STATUS_VALUE))
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC)
                .fullyQualifiedName("Refrenced component nid for semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Refrenced component id", TinkarTerm.PREFERRED)
                .definition("Component id Refrenced", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION)
                .fullyQualifiedName("Refrenced component subtype restriction (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Refrenced component subtype restriction", TinkarTerm.PREFERRED)
                .definition("Stores the (optional) refrenced component type sub restriction selection which will be used by the validator to check the user input for the referenced component when creating an instance of a dynamic field.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROLE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.ROLE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION)
                .fullyQualifiedName("Referenced component type restriction (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Refrenced component type restriction", TinkarTerm.PREFERRED)
                .definition("Stores the (Optional) referenced component type restriction selection which will be used by the validator to check the user input for the referenced component when creating an instance of a dynamic field", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROLE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.ROLE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE)
                .fullyQualifiedName("Regular name description type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Regular name", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.RELATIONSHIP_DESTINATION)
                .fullyQualifiedName("Relationship destination (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Relationship destination", TinkarTerm.PREFERRED)
                .definition("Children? Relationship destination", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RELATIONSHIP_DESTINATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.RELATIONSHIP_ORIGIN)
                .fullyQualifiedName("Relationship origin (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Relationship origin", TinkarTerm.PREFERRED)
                .definition("Parent? Relationship origin", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RELATIONSHIP_ORIGIN.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.ROLE)
                .fullyQualifiedName("Role (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Role", TinkarTerm.PREFERRED)
                .definition("Concept model attribute", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ROLE_GROUP, TinkarTerm.ROLE_TYPE, TinkarTerm.ROLE_OPERATOR), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        starterData.concept(TinkarTerm.ROLE_GROUP)
                .fullyQualifiedName("Role group (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Role group", TinkarTerm.PREFERRED)
                .definition("Attribute?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_GROUP.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROLE))
                .statedDefinition(List.of(TinkarTerm.ROLE))
                .build();

        starterData.concept(TinkarTerm.ROLE_OPERATOR)
                .fullyQualifiedName("Role operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Role operator", TinkarTerm.PREFERRED)
                .definition("Role operator?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.UNIVERSAL_RESTRICTION, TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION, TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION), List.of(TinkarTerm.ROLE))
                .statedDefinition(List.of(TinkarTerm.ROLE))
                .build();

        starterData.concept(TinkarTerm.ROLE_TYPE)
                .fullyQualifiedName("Role type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Role type", TinkarTerm.PREFERRED)
                .definition("Type - attribute?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROLE))
                .statedDefinition(List.of(TinkarTerm.ROLE))
                .build();

        starterData.concept(TinkarTerm.ROLE_TYPE_TO_ADD)
                .fullyQualifiedName("Role type to add (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Role type to add", TinkarTerm.PREFERRED)
                .definition("Action - add role type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_TYPE_TO_ADD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ACTION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.ACTION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName("Root for logic coordinate (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Root", TinkarTerm.PREFERRED)
                .definition("Logic coordinate root", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROOT_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.RUSSIAN_DIALECT)
                .fullyQualifiedName("Russian dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Russian dialect", TinkarTerm.PREFERRED)
                .definition("Russian Dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RUSSIAN_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .statedDefinition(List.of(TinkarTerm.DIALECT_ASSEMBLAGE))
                .build();

        starterData.concept(TinkarTerm.RUSSIAN_LANGUAGE)
                .fullyQualifiedName("Russian language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Russian language", TinkarTerm.PREFERRED)
                .definition("Rusian language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RUSSIAN_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.SANDBOX_COMPONENT)
                .fullyQualifiedName("Sandbox component (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sandbox component", TinkarTerm.PREFERRED)
                .definition("Sandbox componet", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_COMPONENT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.SANDBOX_MODULE, TinkarTerm.SANDBOX_PATH), List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.SANDBOX_MODULE)
                .fullyQualifiedName("Sandbox module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sandbox module", TinkarTerm.PREFERRED)
                .definition("Sandbox module", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_MODULE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.SANDBOX_PATH_MODULE), List.of(TinkarTerm.MODULE, TinkarTerm.SANDBOX_COMPONENT))
                .statedDefinition(List.of(TinkarTerm.MODULE, TinkarTerm.SANDBOX_COMPONENT))
                .build();

        starterData.concept(TinkarTerm.SANDBOX_PATH)
                .fullyQualifiedName("Sandbox path (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sandbox path", TinkarTerm.PREFERRED)
                .definition("Sandbox path", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PATH, TinkarTerm.SANDBOX_COMPONENT))
                .statedDefinition(List.of(TinkarTerm.PATH, TinkarTerm.SANDBOX_COMPONENT))
                .pathMembership()
                .build();

        starterData.concept(TinkarTerm.SANDBOX_PATH_MODULE)
                .fullyQualifiedName("Sandbox path module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sandbox Path module", TinkarTerm.PREFERRED)
                .definition("Sandbox path module", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_PATH_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SANDBOX_MODULE))
                .statedDefinition(List.of(TinkarTerm.SANDBOX_MODULE))
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_FIELD_CONCEPTS)
                .fullyQualifiedName("Semantic field concepts (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Semantic field concepts", TinkarTerm.PREFERRED)
                .definition("Semantic field concepts", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_FIELD_CONCEPTS.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CONCEPT_TYPE))
                .statedDefinition(List.of(TinkarTerm.CONCEPT_TYPE))
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_FIELD_NAME)
                .fullyQualifiedName("Semantic field name (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Field name", TinkarTerm.PREFERRED)
                .definition("Field name - semantics", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_FIELD_NAME.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_FIELD_TYPE)
                .fullyQualifiedName("Semantic field type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Semantic field type", TinkarTerm.PREFERRED)
                .definition("list of fields-  semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_FIELD_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_PROPERTIES)
                .fullyQualifiedName("Semantic properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Semantic properties", TinkarTerm.PREFERRED)
                .definition("the attributes or characteristics of a concept, term, or element that convery meaning or semantics in a given context", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC, TinkarTerm.COMPONENT_FOR_SEMANTIC, TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC, TinkarTerm.SEMANTIC_FIELD_NAME), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_TYPE)
                .fullyQualifiedName("Semantic type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Semantic type", TinkarTerm.PREFERRED)
                .definition("Type- semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_TYPE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.COMPONENT_SEMANTIC, TinkarTerm.CONCEPT_SEMANTIC, TinkarTerm.DESCRIPTION_SEMANTIC, TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC, TinkarTerm.MEMBERSHIP_SEMANTIC), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.SIGNED_INTEGER)
                .fullyQualifiedName("Signed integer (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Signed integer", TinkarTerm.PREFERRED)
                .definition("Signed integer (Foundation metadata concept)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SIGNED_INTEGER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.SPANISH_LANGUAGE)
                .fullyQualifiedName("Spanish langauge (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Spanish language", TinkarTerm.PREFERRED)
                .definition("Spanish Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SPANISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.STANDARD_KOREAN_DIALECT)
                .fullyQualifiedName("Standard Korean dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Standard Korean Dialect", TinkarTerm.PREFERRED)
                .definition("Standard", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STANDARD_KOREAN_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.KOREAN_DIALECT))
                .statedDefinition(List.of(TinkarTerm.KOREAN_DIALECT))
                .build();

        starterData.concept(TinkarTerm.STATED_PREMISE_TYPE)
                .fullyQualifiedName("Stated premise type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Stated", TinkarTerm.PREFERRED)
                .definition("Stated relationship", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STATED_PREMISE_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.AXIOM_ORIGIN))
                .statedDefinition(List.of(TinkarTerm.AXIOM_ORIGIN))
                .build();

        starterData.concept(TinkarTerm.STATUS_FOR_VERSION)
                .fullyQualifiedName("Status for version (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Status for version", TinkarTerm.PREFERRED)
                .definition("Version status?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STATUS_FOR_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.STATUS_VALUE)
                .fullyQualifiedName("Status value (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Status value", TinkarTerm.PREFERRED)
                .definition("Status", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STATUS_VALUE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ACTIVE_STATE, TinkarTerm.CANCELED_STATE, TinkarTerm.INACTIVE_STATE, TinkarTerm.PRIMORDIAL_STATE), List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.STRING)
                .fullyQualifiedName("String (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("String", TinkarTerm.PREFERRED)
                .definition("Data type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STRING.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION)
                .fullyQualifiedName("Sufficient concept definition (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sufficient concept definition", TinkarTerm.PREFERRED)
                .definition("Concept definition - Sufficicent", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR)
                .fullyQualifiedName("Sufficient concept definition operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sufficient concept definition operator", TinkarTerm.PREFERRED)
                .definition("Concept definition operator", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION, TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.SUFFICIENT_SET)
                .fullyQualifiedName("Sufficient set (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sufficient set", TinkarTerm.PREFERRED)
                .definition("Set", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SUFFICIENT_SET.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.EL_PROFILE_SET_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.EL_PROFILE_SET_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.SWEDISH_LANGUAGE)
                .fullyQualifiedName("Swedish language (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Swedish language", TinkarTerm.PREFERRED)
                .definition("Swedish Language", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SWEDISH_LANGUAGE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.LANGUAGE))
                .statedDefinition(List.of(TinkarTerm.LANGUAGE))
                .build();

        starterData.concept(TinkarTerm.TEXT_FOR_DESCRIPTION)
                .fullyQualifiedName("Text for description (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Text", TinkarTerm.PREFERRED)
                .definition("Described text", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TEXT_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION))
                .build();

        starterData.concept(TinkarTerm.TIME_FOR_VERSION)
                .fullyQualifiedName("Time for version (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Time for version", TinkarTerm.PREFERRED)
                .definition("Version time", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TIME_FOR_VERSION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.VERSION_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.VERSION_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.TREE_AMALGAM_PROPERTIES)
                .fullyQualifiedName("Tree amalgam properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Tree amalgam properties (SOLOR)", TinkarTerm.PREFERRED)
                .definition("data structure that conists of nodes connected by edges (a mixture or blend of different elements)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TREE_AMALGAM_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.INVERSE_TREE_LIST, TinkarTerm.TREE_LIST), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.TREE_LIST)
                .fullyQualifiedName("Tree list (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Tree list", TinkarTerm.PREFERRED)
                .definition("List - Tree", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TREE_LIST.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.TREE_AMALGAM_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.TREE_AMALGAM_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.US_ENGLISH_DIALECT)
                .fullyQualifiedName("United States of America English dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("US English dialect", TinkarTerm.PREFERRED)
                .definition("USA -english dialect", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.US_ENGLISH_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.US_NURSING_DIALECT), List.of(englishDialect))
                .statedDefinition(List.of(englishDialect))
                .build();

        starterData.concept(TinkarTerm.US_NURSING_DIALECT)
                .fullyQualifiedName("US Nursing dialect (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("United States English Nursing Dialect", TinkarTerm.PREFERRED)
                .definition("Nursing Dialect -US English", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.US_NURSING_DIALECT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.US_ENGLISH_DIALECT))
                .statedDefinition(List.of(TinkarTerm.US_ENGLISH_DIALECT))
                .build();

        starterData.concept(TinkarTerm.UUID_DATA_TYPE)
                .fullyQualifiedName("UUID data type (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("UUID data type", TinkarTerm.PREFERRED)
                .definition("disctinction of data type of UUID", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UUID_DATA_TYPE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.UUID_FIELD)
                .fullyQualifiedName("UUID field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("UUID field", TinkarTerm.PREFERRED)
                .definition("Universally unique identifier field", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UUID_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.UUID_LIST_FOR_COMPONENT)
                .fullyQualifiedName("UUID list for component (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("UUIDs", TinkarTerm.PREFERRED)
                .definition("UUIDs", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UUID_LIST_FOR_COMPONENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CHRONICLE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.CHRONICLE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.UNCATEGORIZED_PHENOMENON)
                .fullyQualifiedName("Uncategorized phenomenon (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Uncategorized phenomenon", TinkarTerm.PREFERRED)
                .definition("Unknown", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNCATEGORIZED_PHENOMENON.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.PHENOMENON))
                .statedDefinition(List.of(TinkarTerm.PHENOMENON))
                .build();

        starterData.concept(TinkarTerm.UNINITIALIZED_COMPONENT)
                .fullyQualifiedName("Uninitialized Component (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Uninitalized", TinkarTerm.PREFERRED)
                .definition("not initialized component", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNINITIALIZED_COMPONENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.UNIVERSAL_RESTRICTION)
                .fullyQualifiedName("Universal Restriction (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Universal restriction", TinkarTerm.PREFERRED)
                .definition("Restricted universally", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNIVERSAL_RESTRICTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.ROLE_OPERATOR))
                .statedDefinition(List.of(TinkarTerm.ROLE_OPERATOR))
                .build();

        starterData.concept(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                .fullyQualifiedName("Universally Unique Identifier (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Universally Unique Identifier", TinkarTerm.PREFERRED)
                .definition("UUID", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.USER)
                .fullyQualifiedName("User (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("user", TinkarTerm.PREFERRED)
                .definition("Approved users", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.USER.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.KOMET_USER, TinkarTerm.KOMET_USER_LIST, TinkarTerm.MODULE_FOR_USER, TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS, TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS, TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS, TinkarTerm.PATH_FOR_USER), List.of(TinkarTerm.ROOT_VERTEX))
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        starterData.concept(TinkarTerm.VERSION_LIST_FOR_CHRONICLE)
                .fullyQualifiedName("Version list for chronicle (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Versions", TinkarTerm.PREFERRED)
                .definition("Chronicle version list", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERSION_LIST_FOR_CHRONICLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CHRONICLE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.CHRONICLE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.VERSION_PROPERTIES)
                .fullyQualifiedName("Version Properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Version properties", TinkarTerm.PREFERRED)
                .definition("null", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERSION_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.AUTHOR_FOR_VERSION, TinkarTerm.MODULE_FOR_VERSION, TinkarTerm.PATH_FOR_VERSION, TinkarTerm.STATUS_FOR_VERSION, TinkarTerm.TIME_FOR_VERSION, TinkarTerm.DESCRIPTION_VERSION_PROPERTIES), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.VERTEX_FIELD)
                .fullyQualifiedName("Vertex field (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Vertex", TinkarTerm.PREFERRED)
                .definition("Field for Vertex", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERTEX_FIELD.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DISPLAY_FIELDS))
                .statedDefinition(List.of(TinkarTerm.DISPLAY_FIELDS))
                .build();

        starterData.concept(TinkarTerm.VERTEX_SORT)
                .fullyQualifiedName("Vertex sort (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Sort", TinkarTerm.PREFERRED)
                .definition("Vertext sort", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERTEX_SORT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.VERTEX_STATE_SET)
                .fullyQualifiedName("Vertex state set (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Vertex states", TinkarTerm.PREFERRED)
                .definition("Vertext states", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERTEX_STATE_SET.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.VIEW_COORDINATE_KEY)
                .fullyQualifiedName("View coordinate key (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("View Key", TinkarTerm.PREFERRED)
                .definition("View Key", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VIEW_COORDINATE_KEY.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.QUERY_CLAUSES))
                .statedDefinition(List.of(TinkarTerm.QUERY_CLAUSES))
                .build();

        starterData.concept(TinkarTerm.BOOLEAN)
                .fullyQualifiedName("Boolean (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Boolean", TinkarTerm.PREFERRED)
                .definition("Data type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.BYTE_ARRAY)
                .fullyQualifiedName("byte array (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("byte array", TinkarTerm.PREFERRED)
                .definition("Data type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BYTE_ARRAY.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT)
                .fullyQualifiedName("Description list for concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Description list for concept", TinkarTerm.PREFERRED)
                .definition("list of description", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.DOUBLE)
                .fullyQualifiedName("double (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("double", TinkarTerm.PREFERRED)
                .definition("Data type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DOUBLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.FLOAT)
                .fullyQualifiedName("float (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("float", TinkarTerm.PREFERRED)
                .definition("Data type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC)
                .fullyQualifiedName("logic graph for semantic (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Logic graph", TinkarTerm.PREFERRED)
                .definition("Semantic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.SEMANTIC_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LONG)
                .fullyQualifiedName("Long (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Long", TinkarTerm.PREFERRED)
                .definition("Data type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LONG.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .statedDefinition(List.of(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES))
                .build();

        starterData.concept(TinkarTerm.NID)
                .fullyQualifiedName("nid (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("nid", TinkarTerm.PREFERRED)
                .definition("Data type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NID.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.OBJECT))
                .statedDefinition(List.of(TinkarTerm.OBJECT))
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE)
                .fullyQualifiedName("semantic list for chronicle (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("semantic list for chronicle", TinkarTerm.PREFERRED)
                .definition("semantic list", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.CHRONICLE_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.CHRONICLE_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.USERS_MODULE)
                .fullyQualifiedName("users module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("user module", TinkarTerm.PREFERRED)
                .definition("Module - user", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.USERS_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.MODULE))
                .statedDefinition(List.of(TinkarTerm.MODULE))
                .build();

        starterData.concept(TinkarTerm.ROOT_VERTEX)
                .fullyQualifiedName("Integrated Knowledge Management (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Tinkar root concept", TinkarTerm.PREFERRED)
                .definition("Terminologies that are represented in a harmonized manner", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROOT_VERTEX.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.MODEL_CONCEPT, TinkarTerm.MEANING, TinkarTerm.OBJECT, TinkarTerm.ROLE, TinkarTerm.USER, TinkarTerm.ANNOTATION_TYPE, TinkarTerm.CREATIVE_COMMONS_BY_LICENSE, uncategorizedGrouper), null)
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        //Necessary Terms Filtered out in generation routine - START
        starterData.concept(TinkarTerm.QUERY_CLAUSES)
                .fullyQualifiedName("Query clauses (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Query clauses", TinkarTerm.PREFERRED)
                .definition("a distinct component/query that serves a specific purpose", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.QUERY_CLAUSES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.BOOLEAN_REFERENCE, TinkarTerm.VIEW_COORDINATE_KEY), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.FIELD_SUBSTITUTION)
                .fullyQualifiedName("Field substitution (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Field substituion", TinkarTerm.PREFERRED)
                .definition("Replacing a placeholder variable in a field with a specific value", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FIELD_SUBSTITUTION.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.BOOLEAN_SUBSTITUTION, TinkarTerm.CONCEPT_SUBSTITUTION, TinkarTerm.FLOAT_SUBSTITUTION, TinkarTerm.INSTANT_SUBSTITUTION), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.TAXONOMY_OPERATOR)
                .fullyQualifiedName("Taxonomy operator (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Taxonomy operator", TinkarTerm.PREFERRED)
                .definition("an operator or set of operations applied within the contect of a taxonomy", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TAXONOMY_OPERATOR.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.LOGICALLY_EQUIVALENT_TO), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES)
                .fullyQualifiedName("ImmutableCoordinate Properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("ImmutableCoordinate properties", TinkarTerm.PREFERRED)
                .definition("A set of values or data representing a point in space that one established cannot be changed?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IMMUTABLECOORDINATE_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE, TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE, TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE, TinkarTerm.MODULES_FOR_STAMP_COORDINATE, TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE, TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE, TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE, TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE, TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE, TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE, TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE, TinkarTerm.ROOT_FOR_LOGIC_COORDINATE, TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE, TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE, TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE, TinkarTerm.PATH_FOR_PATH_COORDINATE, TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH, TinkarTerm.VERTEX_SORT, TinkarTerm.VERTEX_STATE_SET), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.PURPOSE)
                .fullyQualifiedName("Purpose (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Purpose", TinkarTerm.PREFERRED)
                .definition("Reason", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PURPOSE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.DESCRIPTUM, TinkarTerm.LOGICAL_DEFINITION, TinkarTerm.NAVIGATION), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.ACTION_PROPERTIES)
                .fullyQualifiedName("Action properties (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Action properties", TinkarTerm.PREFERRED)
                .definition("attributes of an action object", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ACTION_PROPERTIES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CONCEPT_CONSTRAINTS, TinkarTerm.CONCEPT_TO_FIND, TinkarTerm.ROLE_TYPE_TO_ADD, TinkarTerm.CONDITIONAL_TRIGGERS), List.of(TinkarTerm.OBJECT_PROPERTIES))
                .statedDefinition(List.of(TinkarTerm.OBJECT_PROPERTIES))
                .build();

        starterData.concept(TinkarTerm.LITERAL_VALUE)
                .fullyQualifiedName("Literal value (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Literal value", TinkarTerm.PREFERRED)
                .definition("Fixed Value/Constant?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LITERAL_VALUE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.BOOLEAN_LITERAL, TinkarTerm.FLOAT_LITERAL, TinkarTerm.INSTANT_LITERAL), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();

        starterData.concept(TinkarTerm.DIALECT_ASSEMBLAGE)
                .fullyQualifiedName("Dialect assemblage (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Dialect assemblage", TinkarTerm.PREFERRED)
                .definition("Language assembling, collection or grouping", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIALECT_ASSEMBLAGE.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.CZECH_DIALECT, englishDialect, TinkarTerm.FRENCH_DIALECT, TinkarTerm.IRISH_DIALECT, TinkarTerm.KOREAN_DIALECT, TinkarTerm.POLISH_DIALECT, TinkarTerm.RUSSIAN_DIALECT), List.of(TinkarTerm.MODEL_CONCEPT))
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES)
                .fullyQualifiedName("Dynamic column data types (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Dynamic column data types", TinkarTerm.PREFERRED)
                .definition("Data storage system where the structure of the data can be altered or extended dynamically?", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DYNAMIC_COLUMN_DATA_TYPES.asUuidArray()[0].toString())
                .statedNavigation(List.of(TinkarTerm.ARRAY, TinkarTerm.BOOLEAN, TinkarTerm.BYTE_ARRAY, TinkarTerm.DOUBLE, TinkarTerm.FLOAT, TinkarTerm.LONG, TinkarTerm.SIGNED_INTEGER, TinkarTerm.STRING, TinkarTerm.UUID_DATA_TYPE), List.of(TinkarTerm.MEANING))
                .statedDefinition(List.of(TinkarTerm.MEANING))
                .build();
        //Necessary Terms Filtered out in generation routine - END

        //Get coordinates to work via Komet's KometTerm
        starterData.concept(TinkarTerm.POSITION_ON_PATH)
                .fullyQualifiedName(TinkarTerm.POSITION_ON_PATH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.POSITION_ON_PATH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.POSITION_ON_PATH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.POSITION_ON_PATH.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName(TinkarTerm.STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym("Stated assemblage", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STATED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName(TinkarTerm.INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym("Inferred assemblage", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INFERRED_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName(TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym("Concepts to classify", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.CLASSIFIER_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName(TinkarTerm.CLASSIFIER_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym("Classifier", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CLASSIFIER_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CLASSIFIER_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName(TinkarTerm.DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym("Dialect order", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIALECT_ASSEMBLAGE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();


        starterData.concept(TinkarTerm.SOLOR_OVERLAY_MODULE)
                .fullyQualifiedName("SOLOR overlay module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("SOLOR overlay module", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SOLOR_OVERLAY_MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SOLOR_OVERLAY_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();


        starterData.concept(TinkarTerm.SOLOR_MODULE)
                .fullyQualifiedName("SOLOR module (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("SOLOR module", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SOLOR_MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SOLOR_MODULE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();


        starterData.concept(TinkarTerm.TRANSITIVE_FEATURE)
                .fullyQualifiedName("Transitive Feature (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Transitive Feature", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.TRANSITIVE_FEATURE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TRANSITIVE_FEATURE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.REFLEXIVE_FEATURE)
                .fullyQualifiedName("Reflexive Feature (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Reflexive Feature", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.REFLEXIVE_FEATURE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFLEXIVE_FEATURE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.LATERALITY)
                .fullyQualifiedName("Laterality (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Laterality", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LATERALITY.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LATERALITY.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.HAS_ACTIVE_INGREDIENT)
                .fullyQualifiedName("Has Active Ingredient (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Has Active Ingredient", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.HAS_ACTIVE_INGREDIENT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.HAS_ACTIVE_INGREDIENT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.HAS_DOSE_FORM)
                .fullyQualifiedName("Has Dose Form (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Has Dose Form", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.HAS_DOSE_FORM.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.HAS_DOSE_FORM.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(uncategorizedGrouper))
                .statedDefinition(List.of(uncategorizedGrouper))
                .build();

        starterData.concept(TinkarTerm.UNMODELED_ROLE_CONCEPT)
                .fullyQualifiedName("Unmodeled role concept (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Unmodeled role concept", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.UNMODELED_ROLE_CONCEPT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNMODELED_ROLE_CONCEPT.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.DYNAMIC_REFERENCED_COMPONENT_RESTRICTION)
                .fullyQualifiedName("Dynamic referenced component restriction (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Dynamic referenced component restriction", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DYNAMIC_REFERENCED_COMPONENT_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DYNAMIC_REFERENCED_COMPONENT_RESTRICTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.EXISTENTIAL_RESTRICTION)
                .fullyQualifiedName("Existential restriction (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Existential restriction", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EXISTENTIAL_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXISTENTIAL_RESTRICTION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.INTRINSIC_ROLE)
                .fullyQualifiedName("Intrinsic role (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Intrinsic role", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.INTRINSIC_ROLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INTRINSIC_ROLE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.PROPERTY_PATTERN_IMPLICATION)
                .fullyQualifiedName("Property pattern implication (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Property pattern implication", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PROPERTY_PATTERN_IMPLICATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PROPERTY_PATTERN_IMPLICATION.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.SNOROCKET_CLASSIFIER)
                .fullyQualifiedName("SnoRocket classifier (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("SnoRocket classifier", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SNOROCKET_CLASSIFIER.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SNOROCKET_CLASSIFIER.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.concept(TinkarTerm.PROPERTY_SET)
                .fullyQualifiedName("Property set (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Property set", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PROPERTY_SET.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PROPERTY_SET.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();


        starterData.concept(TinkarTerm.FEATURE)
                .fullyQualifiedName("Feature (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Feature", TinkarTerm.PREFERRED)
                .definition(TinkarTerm.FEATURE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FEATURE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();


        //Create Description Pattern
        starterData.pattern(TinkarTerm.DESCRIPTION_PATTERN)
                .meaning(TinkarTerm.DESCRIPTION_SEMANTIC)
                .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fieldDefinition(
                        TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION,
                        TinkarTerm.LANGUAGE,
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
                .build();

        //Create Stated Navigation Pattern
        starterData.pattern(TinkarTerm.STATED_NAVIGATION_PATTERN)
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
                .build();

        starterData.concept(StarterDataTerm.STATED_NAVIGATION)
                .fullyQualifiedName("Stated navigation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Stated navigation", TinkarTerm.PREFERRED)
                .definition(StarterDataTerm.STATED_NAVIGATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        //Create Inferred Navigation Pattern
        starterData.pattern(TinkarTerm.INFERRED_NAVIGATION_PATTERN)
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
                .build();


        starterData.concept(StarterDataTerm.INFERRED_NAVIGATION)
                .fullyQualifiedName("Inferred navigation (SOLOR)", TinkarTerm.PREFERRED)
                .synonym("Inferred navigation", TinkarTerm.PREFERRED)
                .definition(StarterDataTerm.INFERRED_NAVIGATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .statedNavigation(null, List.of(TinkarTerm.DESCRIPTION_TYPE))
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        starterData.pattern(StarterDataTerm.INFERRED_DEFINITION_PATTERN)
                .meaning(TinkarTerm.DESCRIPTUM)
                .purpose(TinkarTerm.LOGICAL_DEFINITION)
                .fieldDefinition(
                        TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS,
                        TinkarTerm.MEANING,
                        TinkarTerm.DITREE_FIELD)
                        .build();

        starterData.pattern(StarterDataTerm.STATED_DEFINITION_PATTERN)
                .meaning(TinkarTerm.DESCRIPTUM)
                .purpose(TinkarTerm.LOGICAL_DEFINITION)
                .fieldDefinition(
                        TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS,
                        TinkarTerm.MEANING,
                        TinkarTerm.DITREE_FIELD)
                .build();

        //Create Identifier Pattern
        starterData.pattern(StarterDataTerm.IDENTIFIER_PATTERN)
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.STRING)
                .build();

        //Create US Dialect Pattern
        starterData.pattern(TinkarTerm.US_DIALECT_PATTERN)
                .meaning(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fieldDefinition(
                        TinkarTerm.US_ENGLISH_DIALECT,
                        TinkarTerm.DESCRIPTION_ACCEPTABILITY,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        //Create GB Dialect Pattern
        starterData.pattern(TinkarTerm.GB_DIALECT_PATTERN)
                .meaning(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                .purpose(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fieldDefinition(
                        TinkarTerm.GB_ENGLISH_DIALECT,
                        TinkarTerm.DESCRIPTION_ACCEPTABILITY,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        //Create Axiom Syntax Pattern
        starterData.pattern(StarterDataTerm.AXIOM_SYNTAX_PATTERN)
                .meaning(axiomSyntax)
                .purpose(expressAxiom)
                .fieldDefinition(
                        axiomSyntax,
                        expressAxiom,
                        TinkarTerm.STRING)
                .build();

        //Create Stated Definition Pattern
        starterData.pattern(TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN)
                .meaning(TinkarTerm.DESCRIPTUM)
                .purpose(TinkarTerm.LOGICAL_DEFINITION)
                .fieldDefinition(
                        TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS,
                        TinkarTerm.LOGICAL_DEFINITION,
                        TinkarTerm.DITREE_FIELD)
                .build();

        //Create Inferred Definition Pattern
        starterData.pattern(TinkarTerm.EL_PLUS_PLUS_INFERRED_AXIOMS_PATTERN)
                .meaning(TinkarTerm.DESCRIPTUM)
                .purpose(TinkarTerm.LOGICAL_DEFINITION)
                .fieldDefinition(
                        TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS,
                        TinkarTerm.LOGICAL_DEFINITION,
                        TinkarTerm.DITREE_FIELD)
                .build();

        //Create Path Membership Pattern
        starterData.pattern(StarterDataTerm.PATH_MEMBERSHIP_PATTERN)
                .meaning(TinkarTerm.PATH)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        starterData.pattern(TinkarTerm.SOLOR_CONCEPT_ASSEMBLAGE)
                .meaning(TinkarTerm.CONCEPT_ASSEMBLAGE_FOR_LOGIC_COORDINATE)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();


        //Create STAMP Pattern
        starterData.pattern(TinkarTerm.STAMP_PATTERN)
                .meaning(TinkarTerm.VERSION_PROPERTIES)
                .purpose(TinkarTerm.VERSION_PROPERTIES)
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
                .build();

        //Create Version control path origin pattern
        starterData.pattern(StarterDataTerm.VERSION_CONTROL_PATTERN) //Try TinkarTerm.Path-origins with instant
                .meaning(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH)
                .purpose(TinkarTerm.PATH_ORIGINS)
                .fieldDefinition(
                        TinkarTerm.PATH_CONCEPT,
                        TinkarTerm.PATH_CONCEPT,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.PATH_ORIGINS,
                        TinkarTerm.PATH_ORIGINS,
                        TinkarTerm.STRING)
                .build();

        //Create Comment Pattern
        starterData.pattern(TinkarTerm.COMMENT_PATTERN)
                .meaning(TinkarTerm.COMMENT)
                .purpose(TinkarTerm.COMMENT)
                .fieldDefinition(
                        TinkarTerm.COMMENT,
                        TinkarTerm.COMMENT,
                        TinkarTerm.STRING)
                .build();

        //Create Authoring base model pattern
        starterData.pattern(StarterDataTerm.AUTHORING_BASE_MODEL_PATTERN)
                .meaning(starterDataAuthoring)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        //Create Tinkar Core base model pattern
        starterData.pattern(StarterDataTerm.TINKAR_CORE_BASE_MODEL_PATTERN)
                .meaning(starterDataAuthoring)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        //Create Komet base model component pattern
        starterData.pattern(StarterDataTerm.KOMET_BASE_MODEL_PATTERN)
                .meaning(starterDataAuthoring)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
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
