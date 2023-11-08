package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.util.io.FileUtil;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.entity.transfom.EntityToTinkarSchemaTransformer;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.time.Instant;
import java.util.concurrent.ExecutionException;


public class TinkarStarterData {

    private static File datastore;
    private static File exportFile;

    public static void main(String[] args){
        datastore = new File(args[0]);
        exportFile = new File(args[1]);
        FileUtil.recursiveDelete(datastore);

        UUIDUtility uuidUtility = new UUIDUtility();

        StarterData starterData = new StarterData(datastore, uuidUtility)
                .init()
                .authoringSTAMP(
                        TinkarTerm.ACTIVE_STATE,
                        System.currentTimeMillis(),
                        TinkarTerm.USER,
                        TinkarTerm.PRIMORDIAL_MODULE,
                        TinkarTerm.PRIMORDIAL_PATH);

        configurePatterns(starterData, uuidUtility);
        configureConcepts(starterData);


        starterData.build();

//        exportStarterData();

        starterData.shutdown();
    }

    private static void configurePatterns(StarterData starterData, UUIDUtility uuidUtility){
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

        //Create Identifier Pattern
        starterData.pattern(StarterData.identifierPattern)
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

        //Create Axiom Syntax Pattern
        Concept axiomSyntax = Concept.make("Axiom Syntax", uuidUtility.createUUID("Axiom Syntax"));
        starterData.concept(axiomSyntax)
                .fullyQualifiedName(axiomSyntax.description(), TinkarTerm.PREFERRED)
                .synonym("Axiom Syntax", TinkarTerm.ACCEPTABLE)
                .definition("Syntax defining description logic", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, axiomSyntax.asUuidArray()[0].toString())
                .axiomSyntax("ClassOF(adfasdfasdfasdf)")
                .build();
        Concept expressAxiom = Concept.make("Express axiom syntax", uuidUtility.createUUID("Express axiom syntax"));
        starterData.concept(expressAxiom)
                .fullyQualifiedName(expressAxiom.description(), TinkarTerm.PREFERRED)
                .synonym("Express Axiom", TinkarTerm.ACCEPTABLE)
                .definition("Expressing description logic through syntax", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, expressAxiom.asUuidArray()[0].toString())
                .axiomSyntax("ClassOF(323333j33)")
                .comment("Here's a comment")
                .pathMembership()
                .versionControl(TinkarTerm.HEALTH_CONCEPT, Instant.now().toString())
                .statedDefinition(axiomSyntax)
                .build();
        starterData.pattern(StarterData.axiomSyntaxPattern)
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
        starterData.pattern(StarterData.pathMembershipPattern)
                .meaning(TinkarTerm.PATH)
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

        //Create Tinkar base model component pattern
        starterData.pattern(TinkarTerm.TINKAR_BASE_MODEL_COMPONENT_PATTERN)
                .meaning(TinkarTerm.PATH)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();

        //Create Version control path origin pattern
        starterData.pattern(StarterData.versionControlPathOriginPattern) //Try TinkarTerm.Path-origins with instant
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

        //Create Komet base model component pattern
        starterData.pattern(TinkarTerm.KOMET_BASE_MODEL_COMPONENT_PATTERN)
                .meaning(TinkarTerm.PATH)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();
    }

    private static void configureConcepts(StarterData starterData){
        starterData.concept(TinkarTerm.ACCEPTABLE)
                .fullyQualifiedName(TinkarTerm.ACCEPTABLE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ACCEPTABLE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ACCEPTABLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ACCEPTABLE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ACTIVE_STATE)
                .fullyQualifiedName(TinkarTerm.ACTIVE_STATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ACTIVE_STATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ACTIVE_STATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ACTIVE_STATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE)
                .fullyQualifiedName(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ALLOWED_STATES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.AND)
                .fullyQualifiedName(TinkarTerm.AND.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.AND.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.AND.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AND.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ANNOTATION_TYPE)
                .fullyQualifiedName(TinkarTerm.ANNOTATION_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ANNOTATION_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ANNOTATION_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANNOTATION_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ANONYMOUS_CONCEPT)
                .fullyQualifiedName(TinkarTerm.ANONYMOUS_CONCEPT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ANONYMOUS_CONCEPT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ANONYMOUS_CONCEPT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANONYMOUS_CONCEPT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ANY_COMPONENT)
                .fullyQualifiedName(TinkarTerm.ANY_COMPONENT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ANY_COMPONENT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ANY_COMPONENT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ANY_COMPONENT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ARRAY)
                .fullyQualifiedName(TinkarTerm.ARRAY.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ARRAY.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ARRAY.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ARRAY.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ARRAY_FIELD)
                .fullyQualifiedName(TinkarTerm.ARRAY_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ARRAY_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ARRAY_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ARRAY_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE)
                .fullyQualifiedName(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHOR_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.AUTHOR_FOR_VERSION)
                .fullyQualifiedName(TinkarTerm.AUTHOR_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.AUTHOR_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.AUTHOR_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHOR_FOR_VERSION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE)
                .fullyQualifiedName(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AUTHORS_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.AXIOM_FOCUS)
                .fullyQualifiedName(TinkarTerm.AXIOM_FOCUS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.AXIOM_FOCUS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.AXIOM_FOCUS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AXIOM_FOCUS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.AXIOM_ORIGIN)
                .fullyQualifiedName(TinkarTerm.AXIOM_ORIGIN.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.AXIOM_ORIGIN.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.AXIOM_ORIGIN.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.AXIOM_ORIGIN.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_FIELD)
                .fullyQualifiedName(TinkarTerm.BOOLEAN_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.BOOLEAN_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.BOOLEAN_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_LITERAL)
                .fullyQualifiedName(TinkarTerm.BOOLEAN_LITERAL.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.BOOLEAN_LITERAL.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.BOOLEAN_LITERAL.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_LITERAL.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_REFERENCE)
                .fullyQualifiedName(TinkarTerm.BOOLEAN_REFERENCE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.BOOLEAN_REFERENCE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.BOOLEAN_REFERENCE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_REFERENCE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.BOOLEAN_SUBSTITUTION)
                .fullyQualifiedName(TinkarTerm.BOOLEAN_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.BOOLEAN_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.BOOLEAN_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN_SUBSTITUTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.BYTE_ARRAY_FIELD)
                .fullyQualifiedName(TinkarTerm.BYTE_ARRAY_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.BYTE_ARRAY_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.BYTE_ARRAY_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BYTE_ARRAY_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CANCELED_STATE)
                .fullyQualifiedName(TinkarTerm.CANCELED_STATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CANCELED_STATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CANCELED_STATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CANCELED_STATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CASE_INSENSITIVE_EVALUATION)
                .fullyQualifiedName(TinkarTerm.CASE_INSENSITIVE_EVALUATION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CASE_INSENSITIVE_EVALUATION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CASE_INSENSITIVE_EVALUATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_INSENSITIVE_EVALUATION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CASE_SENSITIVE_EVALUATION)
                .fullyQualifiedName(TinkarTerm.CASE_SENSITIVE_EVALUATION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CASE_SENSITIVE_EVALUATION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CASE_SENSITIVE_EVALUATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_SENSITIVE_EVALUATION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION)
                .fullyQualifiedName(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CASE_SIGNIFICANCE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CHINESE_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.CHINESE_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CHINESE_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CHINESE_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CHINESE_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.COMMENT)
                .fullyQualifiedName(TinkarTerm.COMMENT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.COMMENT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.COMMENT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMMENT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.COMPONENT_ID_LIST_FIELD)
                .fullyQualifiedName(TinkarTerm.COMPONENT_ID_LIST_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.COMPONENT_ID_LIST_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.COMPONENT_ID_LIST_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_ID_LIST_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fullyQualifiedName(TinkarTerm.COMPONENT_ID_SET_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.COMPONENT_ID_SET_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.COMPONENT_ID_SET_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_ID_SET_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.COMPONENT_FIELD)
                .fullyQualifiedName(TinkarTerm.COMPONENT_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.COMPONENT_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.COMPONENT_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.COMPONENT_FOR_SEMANTIC)
                .fullyQualifiedName(TinkarTerm.COMPONENT_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.COMPONENT_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.COMPONENT_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_FOR_SEMANTIC.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.COMPONENT_SEMANTIC)
                .fullyQualifiedName(TinkarTerm.COMPONENT_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.COMPONENT_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.COMPONENT_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_SEMANTIC.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.COMPONENT_TYPE_FOCUS)
                .fullyQualifiedName(TinkarTerm.COMPONENT_TYPE_FOCUS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.COMPONENT_TYPE_FOCUS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.COMPONENT_TYPE_FOCUS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.COMPONENT_TYPE_FOCUS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_CONSTRAINTS)
                .fullyQualifiedName(TinkarTerm.CONCEPT_CONSTRAINTS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_CONSTRAINTS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_CONSTRAINTS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_CONSTRAINTS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE)
                .fullyQualifiedName(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_DETAILS_TREE_TABLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_DETAILS_TREE_TABLE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_FIELD)
                .fullyQualifiedName(TinkarTerm.CONCEPT_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_FOCUS)
                .fullyQualifiedName(TinkarTerm.CONCEPT_FOCUS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_FOCUS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_FOCUS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_FOCUS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_REFERENCE)
                .fullyQualifiedName(TinkarTerm.CONCEPT_REFERENCE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_REFERENCE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_REFERENCE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_REFERENCE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_SEMANTIC)
                .fullyQualifiedName(TinkarTerm.CONCEPT_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_SEMANTIC.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_SUBSTITUTION)
                .fullyQualifiedName(TinkarTerm.CONCEPT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_SUBSTITUTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_TO_FIND)
                .fullyQualifiedName(TinkarTerm.CONCEPT_TO_FIND.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_TO_FIND.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_TO_FIND.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_TO_FIND.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_TYPE)
                .fullyQualifiedName(TinkarTerm.CONCEPT_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCEPT_VERSION)
                .fullyQualifiedName(TinkarTerm.CONCEPT_VERSION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCEPT_VERSION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCEPT_VERSION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCEPT_VERSION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONCRETE_DOMAIN_OPERATOR)
                .fullyQualifiedName(TinkarTerm.CONCRETE_DOMAIN_OPERATOR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONCRETE_DOMAIN_OPERATOR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONCRETE_DOMAIN_OPERATOR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONCRETE_DOMAIN_OPERATOR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONDITIONAL_TRIGGERS)
                .fullyQualifiedName(TinkarTerm.CONDITIONAL_TRIGGERS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONDITIONAL_TRIGGERS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONDITIONAL_TRIGGERS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONDITIONAL_TRIGGERS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CONNECTIVE_OPERATOR)
                .fullyQualifiedName(TinkarTerm.CONNECTIVE_OPERATOR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CONNECTIVE_OPERATOR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CONNECTIVE_OPERATOR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CONNECTIVE_OPERATOR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CORELATION_EXPRESSION)
                .fullyQualifiedName(TinkarTerm.CORELATION_EXPRESSION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CORELATION_EXPRESSION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CORELATION_EXPRESSION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CORELATION_EXPRESSION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CORELATION_REFERENCE_EXPRESSION)
                .fullyQualifiedName(TinkarTerm.CORELATION_REFERENCE_EXPRESSION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CORELATION_REFERENCE_EXPRESSION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CORELATION_REFERENCE_EXPRESSION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CORELATION_REFERENCE_EXPRESSION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE)
                .fullyQualifiedName(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CREATIVE_COMMONS_BY_LICENSE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CREATIVE_COMMONS_BY_LICENSE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CZECH_DIALECT)
                .fullyQualifiedName(TinkarTerm.CZECH_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CZECH_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CZECH_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CZECH_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.CZECH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.CZECH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.CZECH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.CZECH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.CZECH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DANISH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.DANISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DANISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DANISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DANISH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE)
                .fullyQualifiedName(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFAULT_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DEFINITION_DESCRIPTION_TYPE)
                .fullyQualifiedName(TinkarTerm.DEFINITION_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DEFINITION_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DEFINITION_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFINITION_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DEFINITION_ROOT)
                .fullyQualifiedName(TinkarTerm.DEFINITION_ROOT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DEFINITION_ROOT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DEFINITION_ROOT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEFINITION_ROOT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_ACCEPTABILITY)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_ACCEPTABILITY.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_ACCEPTABILITY.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_ACCEPTABILITY.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_ACCEPTABILITY.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CASE_SENSITIVE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CASE_SENSITIVE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CASE_SIGNIFICANCE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_CORE_TYPE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_CORE_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_CORE_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_CORE_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_CORE_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_DIALECT_PAIR)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_DIALECT_PAIR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_DIALECT_PAIR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_DIALECT_PAIR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_DIALECT_PAIR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_FOCUS)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_FOCUS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_FOCUS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_FOCUS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_FOCUS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_INITIAL_CHARACTER_CASE_SENSITIVE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_LOGIC_PROFILE_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_SEMANTIC)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_SEMANTIC.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_TYPE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_LOGIC_PROFILE)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_LOGIC_PROFILE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_LOGIC_PROFILE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_LOGIC_PROFILE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_LOGIC_PROFILE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTUM)
                .fullyQualifiedName(TinkarTerm.DESCRIPTUM.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTUM.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTUM.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTUM.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE)
                .fullyQualifiedName(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESTINATION_MODULE_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DEVELOPMENT_MODULE)
                .fullyQualifiedName(TinkarTerm.DEVELOPMENT_MODULE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DEVELOPMENT_MODULE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DEVELOPMENT_MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEVELOPMENT_MODULE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DEVELOPMENT_PATH)
                .fullyQualifiedName(TinkarTerm.DEVELOPMENT_PATH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DEVELOPMENT_PATH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DEVELOPMENT_PATH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DEVELOPMENT_PATH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DIGRAPH_FIELD)
                .fullyQualifiedName(TinkarTerm.DIGRAPH_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DIGRAPH_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DIGRAPH_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIGRAPH_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DITREE_FIELD)
                .fullyQualifiedName(TinkarTerm.DITREE_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DITREE_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DITREE_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DITREE_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR)
                .fullyQualifiedName(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIALECT_FOR_DIALECT_AND_OR_DESCRIPTION_PAIR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIGRAPH_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DIRECTED_GRAPH)
                .fullyQualifiedName(TinkarTerm.DIRECTED_GRAPH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DIRECTED_GRAPH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DIRECTED_GRAPH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DIRECTED_GRAPH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DISJOINT_WITH)
                .fullyQualifiedName(TinkarTerm.DISJOINT_WITH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DISJOINT_WITH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DISJOINT_WITH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DISJOINT_WITH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DISPLAY_FIELDS)
                .fullyQualifiedName(TinkarTerm.DISPLAY_FIELDS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DISPLAY_FIELDS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DISPLAY_FIELDS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DISPLAY_FIELDS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DOUBLE_FIELD)
                .fullyQualifiedName(TinkarTerm.DOUBLE_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DOUBLE_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DOUBLE_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DOUBLE_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DUTCH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.DUTCH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DUTCH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DUTCH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DUTCH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EL_PROFILE_SET_OPERATOR)
                .fullyQualifiedName(TinkarTerm.EL_PROFILE_SET_OPERATOR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EL_PROFILE_SET_OPERATOR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EL_PROFILE_SET_OPERATOR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PROFILE_SET_OPERATOR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION)
                .fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_INFERRED_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS)
                .fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_INFERRED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION)
                .fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_STATED_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS)
                .fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_STATED_TERMINOLOGICAL_AXIOMS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_DIGRAPH)
                .fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_DIGRAPH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EL_PLUS_PLUS_DIGRAPH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EL_PLUS_PLUS_DIGRAPH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_DIGRAPH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EL_PLUS_PLUS_PROFILE)
                .fullyQualifiedName(TinkarTerm.EL_PLUS_PLUS_PROFILE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EL_PLUS_PLUS_PROFILE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EL_PLUS_PLUS_PROFILE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EL_PLUS_PLUS_PROFILE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ENGLISH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.ENGLISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ENGLISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ENGLISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ENGLISH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EQUAL_TO)
                .fullyQualifiedName(TinkarTerm.EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EQUAL_TO.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EXACT)
                .fullyQualifiedName(TinkarTerm.EXACT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EXACT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EXACT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXACT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EXTENDED_DESCRIPTION_TYPE)
                .fullyQualifiedName(TinkarTerm.EXTENDED_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EXTENDED_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EXTENDED_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXTENDED_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE)
                .fullyQualifiedName(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.EXTENDED_RELATIONSHIP_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.EXTENDED_RELATIONSHIP_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.FLOAT_FIELD)
                .fullyQualifiedName(TinkarTerm.FLOAT_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.FLOAT_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.FLOAT_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.FLOAT_LITERAL)
                .fullyQualifiedName(TinkarTerm.FLOAT_LITERAL.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.FLOAT_LITERAL.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.FLOAT_LITERAL.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_LITERAL.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.FLOAT_SUBSTITUTION)
                .fullyQualifiedName(TinkarTerm.FLOAT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.FLOAT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.FLOAT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT_SUBSTITUTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.FRENCH_DIALECT)
                .fullyQualifiedName(TinkarTerm.FRENCH_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.FRENCH_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.FRENCH_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FRENCH_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.FRENCH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.FRENCH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.FRENCH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.FRENCH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FRENCH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE)
                .fullyQualifiedName(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.GB_ENGLISH_DIALECT)
                .fullyQualifiedName(TinkarTerm.GB_ENGLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.GB_ENGLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.GB_ENGLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GB_ENGLISH_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.GERMAN_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.GERMAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.GERMAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.GERMAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GERMAN_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.GREATER_THAN)
                .fullyQualifiedName(TinkarTerm.GREATER_THAN.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.GREATER_THAN.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.GREATER_THAN.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GREATER_THAN.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.GREATER_THAN_OR_EQUAL_TO)
                .fullyQualifiedName(TinkarTerm.GREATER_THAN_OR_EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.GREATER_THAN_OR_EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.GREATER_THAN_OR_EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GREATER_THAN_OR_EQUAL_TO.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.GROUPING)
                .fullyQualifiedName(TinkarTerm.GROUPING.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.GROUPING.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.GROUPING.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.GROUPING.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.HEALTH_CONCEPT)
                .fullyQualifiedName(TinkarTerm.HEALTH_CONCEPT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.HEALTH_CONCEPT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.HEALTH_CONCEPT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.HEALTH_CONCEPT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.INACTIVE_STATE)
                .fullyQualifiedName(TinkarTerm.INACTIVE_STATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.INACTIVE_STATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.INACTIVE_STATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INACTIVE_STATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.INFERRED_PREMISE_TYPE)
                .fullyQualifiedName(TinkarTerm.INFERRED_PREMISE_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.INFERRED_PREMISE_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.INFERRED_PREMISE_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INFERRED_PREMISE_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.INSTANT_LITERAL)
                .fullyQualifiedName(TinkarTerm.INSTANT_LITERAL.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.INSTANT_LITERAL.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.INSTANT_LITERAL.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INSTANT_LITERAL.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.INSTANT_SUBSTITUTION)
                .fullyQualifiedName(TinkarTerm.INSTANT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.INSTANT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.INSTANT_SUBSTITUTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INSTANT_SUBSTITUTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.INVERSE_NAME)
                .fullyQualifiedName(TinkarTerm.INVERSE_NAME.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.INVERSE_NAME.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.INVERSE_NAME.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INVERSE_NAME.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.INVERSE_TREE_LIST)
                .fullyQualifiedName(TinkarTerm.INVERSE_TREE_LIST.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.INVERSE_TREE_LIST.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.INVERSE_TREE_LIST.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.INVERSE_TREE_LIST.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.IRISH_DIALECT)
                .fullyQualifiedName(TinkarTerm.IRISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.IRISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.IRISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IRISH_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.IRISH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.IRISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.IRISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.IRISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IRISH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.IS_A)
                .fullyQualifiedName(TinkarTerm.IS_A.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.IS_A.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.IS_A.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.IS_A_INFERRED_NAVIGATION)
                .fullyQualifiedName(TinkarTerm.IS_A_INFERRED_NAVIGATION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.IS_A_INFERRED_NAVIGATION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.IS_A_INFERRED_NAVIGATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A_INFERRED_NAVIGATION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.IS_A_STATED_NAVIGATION)
                .fullyQualifiedName(TinkarTerm.IS_A_STATED_NAVIGATION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.IS_A_STATED_NAVIGATION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.IS_A_STATED_NAVIGATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IS_A_STATED_NAVIGATION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ITALIAN_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.ITALIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ITALIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ITALIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ITALIAN_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.KOMET_MODULE)
                .fullyQualifiedName(TinkarTerm.KOMET_MODULE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.KOMET_MODULE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.KOMET_MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_MODULE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.KOMET_USER)
                .fullyQualifiedName(TinkarTerm.KOMET_USER.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.KOMET_USER.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.KOMET_USER.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_USER.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.KOMET_USER_LIST)
                .fullyQualifiedName(TinkarTerm.KOMET_USER_LIST.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.KOMET_USER_LIST.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.KOMET_USER_LIST.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_USER_LIST.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.KOMET_ISSUE)
                .fullyQualifiedName(TinkarTerm.KOMET_ISSUE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.KOMET_ISSUE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.KOMET_ISSUE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOMET_ISSUE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.KOREAN_DIALECT)
                .fullyQualifiedName(TinkarTerm.KOREAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.KOREAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.KOREAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOREAN_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.KOREAN_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.KOREAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.KOREAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.KOREAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.KOREAN_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LANGUAGE)
                .fullyQualifiedName(TinkarTerm.LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION)
                .fullyQualifiedName(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_CONCEPT_NID_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_COORDINATE_NAME)
                .fullyQualifiedName(TinkarTerm.LANGUAGE_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LANGUAGE_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LANGUAGE_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_COORDINATE_NAME.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_NID_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LANGUAGE_SPECIFICATION_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LESS_THAN)
                .fullyQualifiedName(TinkarTerm.LESS_THAN.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LESS_THAN.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LESS_THAN.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LESS_THAN.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LESS_THAN_OR_EQUAL_TO)
                .fullyQualifiedName(TinkarTerm.LESS_THAN_OR_EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LESS_THAN_OR_EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LESS_THAN_OR_EQUAL_TO.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LESS_THAN_OR_EQUAL_TO.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LITHUANIAN_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.LITHUANIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LITHUANIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LITHUANIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LITHUANIAN_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LOGIC_COORDINATE_NAME)
                .fullyQualifiedName(TinkarTerm.LOGIC_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LOGIC_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LOGIC_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGIC_COORDINATE_NAME.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LOGICAL_DEFINITION)
                .fullyQualifiedName(TinkarTerm.LOGICAL_DEFINITION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LOGICAL_DEFINITION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LOGICAL_DEFINITION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_DEFINITION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LOGICAL_EXPRESSION_FIELD)
                .fullyQualifiedName(TinkarTerm.LOGICAL_EXPRESSION_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LOGICAL_EXPRESSION_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LOGICAL_EXPRESSION_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_EXPRESSION_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC)
                .fullyQualifiedName(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICAL_EXPRESSION_SEMANTIC.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LOGICALLY_EQUIVALENT_TO)
                .fullyQualifiedName(TinkarTerm.LOGICALLY_EQUIVALENT_TO.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LOGICALLY_EQUIVALENT_TO.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LOGICALLY_EQUIVALENT_TO.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGICALLY_EQUIVALENT_TO.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MASTER_PATH)
                .fullyQualifiedName(TinkarTerm.MASTER_PATH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MASTER_PATH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MASTER_PATH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MASTER_PATH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MEANING)
                .fullyQualifiedName(TinkarTerm.MEANING.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MEANING.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MEANING.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MEANING.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .fullyQualifiedName(TinkarTerm.MEMBERSHIP_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MEMBERSHIP_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MEMBERSHIP_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MEMBERSHIP_SEMANTIC.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULE)
                .fullyQualifiedName(TinkarTerm.MODULE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE)
                .fullyQualifiedName(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_EXCLUSION_SET_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULE_FOR_USER)
                .fullyQualifiedName(TinkarTerm.MODULE_FOR_USER.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULE_FOR_USER.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULE_FOR_USER.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_FOR_USER.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULE_FOR_VERSION)
                .fullyQualifiedName(TinkarTerm.MODULE_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULE_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULE_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_FOR_VERSION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE)
                .fullyQualifiedName(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_OPTIONS_FOR_EDIT_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE)
                .fullyQualifiedName(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_LANGUAGE_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE)
                .fullyQualifiedName(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_LIST_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE)
                .fullyQualifiedName(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULE_PREFERENCE_ORDER_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.MODULES_FOR_STAMP_COORDINATE)
                .fullyQualifiedName(TinkarTerm.MODULES_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.MODULES_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.MODULES_FOR_STAMP_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.MODULES_FOR_STAMP_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.NAVIGATION)
                .fullyQualifiedName(TinkarTerm.NAVIGATION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.NAVIGATION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.NAVIGATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.NAVIGATION_CONCEPT_SET)
                .fullyQualifiedName(TinkarTerm.NAVIGATION_CONCEPT_SET.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.NAVIGATION_CONCEPT_SET.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.NAVIGATION_CONCEPT_SET.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION_CONCEPT_SET.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.NAVIGATION_PATTERN)
                .fullyQualifiedName(TinkarTerm.NAVIGATION_PATTERN.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.NAVIGATION_PATTERN.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.NAVIGATION_PATTERN.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION_PATTERN.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.NAVIGATION_VERTEX)
                .fullyQualifiedName(TinkarTerm.NAVIGATION_VERTEX.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.NAVIGATION_VERTEX.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.NAVIGATION_VERTEX.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NAVIGATION_VERTEX.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION)
                .fullyQualifiedName(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NECESSARY_BUT_NOT_SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.NECESSARY_SET)
                .fullyQualifiedName(TinkarTerm.NECESSARY_SET.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.NECESSARY_SET.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.NECESSARY_SET.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NECESSARY_SET.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.NOT_APPLICABLE)
                .fullyQualifiedName(TinkarTerm.NOT_APPLICABLE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.NOT_APPLICABLE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.NOT_APPLICABLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NOT_APPLICABLE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.OBJECT)
                .fullyQualifiedName(TinkarTerm.OBJECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.OBJECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.OBJECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.OBJECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.OR)
                .fullyQualifiedName(TinkarTerm.OR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.OR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.OR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.OR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS)
                .fullyQualifiedName(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_AXIOM_ATTACHMENTS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS)
                .fullyQualifiedName(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_CONCEPT_ATTACHMENTS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS)
                .fullyQualifiedName(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ORDER_FOR_DESCRIPTION_ATTACHMENTS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PART_OF)
                .fullyQualifiedName(TinkarTerm.PART_OF.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PART_OF.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PART_OF.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PART_OF.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PARTIAL)
                .fullyQualifiedName(TinkarTerm.PARTIAL.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PARTIAL.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PARTIAL.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PARTIAL.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH)
                .fullyQualifiedName(TinkarTerm.PATH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_CONCEPT)
                .fullyQualifiedName(TinkarTerm.PATH_CONCEPT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_CONCEPT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_CONCEPT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_CONCEPT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_COORDINATE_NAME)
                .fullyQualifiedName(TinkarTerm.PATH_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_COORDINATE_NAME.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_COORDINATE_NAME.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_PATH_COORDINATE)
                .fullyQualifiedName(TinkarTerm.PATH_FOR_PATH_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_FOR_PATH_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_FOR_PATH_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_PATH_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_USER)
                .fullyQualifiedName(TinkarTerm.PATH_FOR_USER.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_FOR_USER.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_FOR_USER.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_USER.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_FOR_VERSION)
                .fullyQualifiedName(TinkarTerm.PATH_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_FOR_VERSION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE)
                .fullyQualifiedName(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_OPTIONS_FOR_EDIT_CORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_ORIGINS)
                .fullyQualifiedName(TinkarTerm.PATH_ORIGINS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_ORIGINS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_ORIGINS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_ORIGINS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_ORIGINS_PATTERN)
                .fullyQualifiedName(TinkarTerm.PATH_ORIGINS_PATTERN.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_ORIGINS_PATTERN.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_ORIGINS_PATTERN.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_ORIGINS_PATTERN.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH)
                .fullyQualifiedName(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATH_ORIGINS_FOR_STAMP_PATH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PATHS_PATTERN)
                .fullyQualifiedName(TinkarTerm.PATHS_PATTERN.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PATHS_PATTERN.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PATHS_PATTERN.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PATHS_PATTERN.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PHENOMENON)
                .fullyQualifiedName(TinkarTerm.PHENOMENON.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PHENOMENON.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PHENOMENON.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PHENOMENON.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.POLISH_DIALECT)
                .fullyQualifiedName(TinkarTerm.POLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.POLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.POLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.POLISH_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.POLISH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.POLISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.POLISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.POLISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.POLISH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PREFERRED)
                .fullyQualifiedName(TinkarTerm.PREFERRED.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PREFERRED.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PREFERRED.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PREFERRED.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PRESENTATION_UNIT_DIFFERENT)
                .fullyQualifiedName(TinkarTerm.PRESENTATION_UNIT_DIFFERENT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PRESENTATION_UNIT_DIFFERENT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PRESENTATION_UNIT_DIFFERENT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRESENTATION_UNIT_DIFFERENT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE)
                .fullyQualifiedName(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_UUID_FOR_CHRONICLE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_MODULE)
                .fullyQualifiedName(TinkarTerm.PRIMORDIAL_MODULE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PRIMORDIAL_MODULE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PRIMORDIAL_MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_MODULE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_PATH)
                .fullyQualifiedName(TinkarTerm.PRIMORDIAL_PATH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PRIMORDIAL_PATH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PRIMORDIAL_PATH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_PATH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.PRIMORDIAL_STATE)
                .fullyQualifiedName(TinkarTerm.PRIMORDIAL_STATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.PRIMORDIAL_STATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.PRIMORDIAL_STATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.PRIMORDIAL_STATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC)
                .fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_NID_FOR_SEMANTIC.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION)
                .fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_SUBTYPE_RESTRICTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION)
                .fullyQualifiedName(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REFERENCED_COMPONENT_TYPE_RESTRICTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE)
                .fullyQualifiedName(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.RELATIONSHIP_DESTINATION)
                .fullyQualifiedName(TinkarTerm.RELATIONSHIP_DESTINATION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.RELATIONSHIP_DESTINATION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.RELATIONSHIP_DESTINATION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RELATIONSHIP_DESTINATION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.RELATIONSHIP_ORIGIN)
                .fullyQualifiedName(TinkarTerm.RELATIONSHIP_ORIGIN.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.RELATIONSHIP_ORIGIN.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.RELATIONSHIP_ORIGIN.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RELATIONSHIP_ORIGIN.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ROLE)
                .fullyQualifiedName(TinkarTerm.ROLE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ROLE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ROLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ROLE_GROUP)
                .fullyQualifiedName(TinkarTerm.ROLE_GROUP.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ROLE_GROUP.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ROLE_GROUP.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_GROUP.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ROLE_OPERATOR)
                .fullyQualifiedName(TinkarTerm.ROLE_OPERATOR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ROLE_OPERATOR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ROLE_OPERATOR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_OPERATOR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ROLE_TYPE)
                .fullyQualifiedName(TinkarTerm.ROLE_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ROLE_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ROLE_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ROLE_TYPE_TO_ADD)
                .fullyQualifiedName(TinkarTerm.ROLE_TYPE_TO_ADD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ROLE_TYPE_TO_ADD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ROLE_TYPE_TO_ADD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROLE_TYPE_TO_ADD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE)
                .fullyQualifiedName(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ROOT_FOR_LOGIC_COORDINATE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROOT_FOR_LOGIC_COORDINATE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.RUSSIAN_DIALECT)
                .fullyQualifiedName(TinkarTerm.RUSSIAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.RUSSIAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.RUSSIAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RUSSIAN_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.RUSSIAN_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.RUSSIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.RUSSIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.RUSSIAN_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.RUSSIAN_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SANDBOX_COMPONENT)
                .fullyQualifiedName(TinkarTerm.SANDBOX_COMPONENT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SANDBOX_COMPONENT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SANDBOX_COMPONENT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_COMPONENT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SANDBOX_MODULE)
                .fullyQualifiedName(TinkarTerm.SANDBOX_MODULE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SANDBOX_MODULE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SANDBOX_MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_MODULE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SANDBOX_PATH)
                .fullyQualifiedName(TinkarTerm.SANDBOX_PATH.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SANDBOX_PATH.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SANDBOX_PATH.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_PATH.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SANDBOX_PATH_MODULE)
                .fullyQualifiedName(TinkarTerm.SANDBOX_PATH_MODULE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SANDBOX_PATH_MODULE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SANDBOX_PATH_MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SANDBOX_PATH_MODULE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_FIELD_CONCEPTS)
                .fullyQualifiedName(TinkarTerm.SEMANTIC_FIELD_CONCEPTS.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SEMANTIC_FIELD_CONCEPTS.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SEMANTIC_FIELD_CONCEPTS.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_FIELD_CONCEPTS.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_FIELD_NAME)
                .fullyQualifiedName(TinkarTerm.SEMANTIC_FIELD_NAME.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SEMANTIC_FIELD_NAME.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SEMANTIC_FIELD_NAME.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_FIELD_NAME.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_FIELD_TYPE)
                .fullyQualifiedName(TinkarTerm.SEMANTIC_FIELD_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SEMANTIC_FIELD_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SEMANTIC_FIELD_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_FIELD_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_TYPE)
                .fullyQualifiedName(TinkarTerm.SEMANTIC_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SEMANTIC_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SEMANTIC_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SIGNED_INTEGER)
                .fullyQualifiedName(TinkarTerm.SIGNED_INTEGER.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SIGNED_INTEGER.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SIGNED_INTEGER.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SIGNED_INTEGER.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SPANISH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.SPANISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SPANISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SPANISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SPANISH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.STANDARD_KOREAN_DIALECT)
                .fullyQualifiedName(TinkarTerm.STANDARD_KOREAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.STANDARD_KOREAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.STANDARD_KOREAN_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STANDARD_KOREAN_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.STATED_PREMISE_TYPE)
                .fullyQualifiedName(TinkarTerm.STATED_PREMISE_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.STATED_PREMISE_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.STATED_PREMISE_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STATED_PREMISE_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.STATUS_FOR_VERSION)
                .fullyQualifiedName(TinkarTerm.STATUS_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.STATUS_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.STATUS_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STATUS_FOR_VERSION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.STRING)
                .fullyQualifiedName(TinkarTerm.STRING.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.STRING.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.STRING.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STRING.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION)
                .fullyQualifiedName(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR)
                .fullyQualifiedName(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SUFFICIENT_CONCEPT_DEFINITION_OPERATOR.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SUFFICIENT_SET)
                .fullyQualifiedName(TinkarTerm.SUFFICIENT_SET.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SUFFICIENT_SET.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SUFFICIENT_SET.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SUFFICIENT_SET.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SWEDISH_LANGUAGE)
                .fullyQualifiedName(TinkarTerm.SWEDISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SWEDISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SWEDISH_LANGUAGE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SWEDISH_LANGUAGE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.TEXT_FOR_DESCRIPTION)
                .fullyQualifiedName(TinkarTerm.TEXT_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.TEXT_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.TEXT_FOR_DESCRIPTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TEXT_FOR_DESCRIPTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.TIME_FOR_VERSION)
                .fullyQualifiedName(TinkarTerm.TIME_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.TIME_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.TIME_FOR_VERSION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TIME_FOR_VERSION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.TREE_LIST)
                .fullyQualifiedName(TinkarTerm.TREE_LIST.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.TREE_LIST.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.TREE_LIST.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.TREE_LIST.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.US_ENGLISH_DIALECT)
                .fullyQualifiedName(TinkarTerm.US_ENGLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.US_ENGLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.US_ENGLISH_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.US_ENGLISH_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.US_NURSING_DIALECT)
                .fullyQualifiedName(TinkarTerm.US_NURSING_DIALECT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.US_NURSING_DIALECT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.US_NURSING_DIALECT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.US_NURSING_DIALECT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.UUID_DATA_TYPE)
                .fullyQualifiedName(TinkarTerm.UUID_DATA_TYPE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.UUID_DATA_TYPE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.UUID_DATA_TYPE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UUID_DATA_TYPE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.UUID_FIELD)
                .fullyQualifiedName(TinkarTerm.UUID_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.UUID_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.UUID_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UUID_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.UUID_LIST_FOR_COMPONENT)
                .fullyQualifiedName(TinkarTerm.UUID_LIST_FOR_COMPONENT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.UUID_LIST_FOR_COMPONENT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.UUID_LIST_FOR_COMPONENT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UUID_LIST_FOR_COMPONENT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.UNCATEGORIZED_PHENOMENON)
                .fullyQualifiedName(TinkarTerm.UNCATEGORIZED_PHENOMENON.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.UNCATEGORIZED_PHENOMENON.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.UNCATEGORIZED_PHENOMENON.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNCATEGORIZED_PHENOMENON.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.UNINITIALIZED_COMPONENT)
                .fullyQualifiedName(TinkarTerm.UNINITIALIZED_COMPONENT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.UNINITIALIZED_COMPONENT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.UNINITIALIZED_COMPONENT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNINITIALIZED_COMPONENT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.UNIVERSAL_RESTRICTION)
                .fullyQualifiedName(TinkarTerm.UNIVERSAL_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.UNIVERSAL_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.UNIVERSAL_RESTRICTION.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNIVERSAL_RESTRICTION.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                .fullyQualifiedName(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.USER)
                .fullyQualifiedName(TinkarTerm.USER.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.USER.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.USER.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.USER.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.VERSION_LIST_FOR_CHRONICLE)
                .fullyQualifiedName(TinkarTerm.VERSION_LIST_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.VERSION_LIST_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.VERSION_LIST_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERSION_LIST_FOR_CHRONICLE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.VERTEX_FIELD)
                .fullyQualifiedName(TinkarTerm.VERTEX_FIELD.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.VERTEX_FIELD.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.VERTEX_FIELD.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERTEX_FIELD.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.VERTEX_SORT)
                .fullyQualifiedName(TinkarTerm.VERTEX_SORT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.VERTEX_SORT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.VERTEX_SORT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERTEX_SORT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.VERTEX_STATE_SET)
                .fullyQualifiedName(TinkarTerm.VERTEX_STATE_SET.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.VERTEX_STATE_SET.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.VERTEX_STATE_SET.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERTEX_STATE_SET.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.VIEW_COORDINATE_KEY)
                .fullyQualifiedName(TinkarTerm.VIEW_COORDINATE_KEY.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.VIEW_COORDINATE_KEY.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.VIEW_COORDINATE_KEY.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VIEW_COORDINATE_KEY.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.BOOLEAN)
                .fullyQualifiedName(TinkarTerm.BOOLEAN.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.BOOLEAN.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.BOOLEAN.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BOOLEAN.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.BYTE_ARRAY)
                .fullyQualifiedName(TinkarTerm.BYTE_ARRAY.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.BYTE_ARRAY.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.BYTE_ARRAY.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.BYTE_ARRAY.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT)
                .fullyQualifiedName(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DESCRIPTION_LIST_FOR_CONCEPT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.DOUBLE)
                .fullyQualifiedName(TinkarTerm.DOUBLE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.DOUBLE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.DOUBLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.DOUBLE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.FLOAT)
                .fullyQualifiedName(TinkarTerm.FLOAT.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.FLOAT.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.FLOAT.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.FLOAT.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC)
                .fullyQualifiedName(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LOGIC_GRAPH_FOR_SEMANTIC.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.LONG)
                .fullyQualifiedName(TinkarTerm.LONG.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.LONG.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.LONG.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.LONG.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.NID)
                .fullyQualifiedName(TinkarTerm.NID.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.NID.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.NID.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.NID.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE)
                .fullyQualifiedName(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.SEMANTIC_LIST_FOR_CHRONICLE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.USERS_MODULE)
                .fullyQualifiedName(TinkarTerm.USERS_MODULE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.USERS_MODULE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.USERS_MODULE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.USERS_MODULE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.ROOT_VERTEX)
                .fullyQualifiedName(TinkarTerm.ROOT_VERTEX.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.ROOT_VERTEX.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.ROOT_VERTEX.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.ROOT_VERTEX.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.IDENTIFIER_SOURCE)
                .fullyQualifiedName(TinkarTerm.IDENTIFIER_SOURCE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.IDENTIFIER_SOURCE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.IDENTIFIER_SOURCE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.IDENTIFIER_SOURCE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.VERSION_PROPERTIES)
                .fullyQualifiedName(TinkarTerm.VERSION_PROPERTIES.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.VERSION_PROPERTIES.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.VERSION_PROPERTIES.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.VERSION_PROPERTIES.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
                .build();

        starterData.concept(TinkarTerm.STATUS_VALUE)
                .fullyQualifiedName(TinkarTerm.STATUS_VALUE.description(), TinkarTerm.PREFERRED)
                .synonym(TinkarTerm.STATUS_VALUE.description(), TinkarTerm.PREFERRED)
                .definition(TinkarTerm.STATUS_VALUE.description(), TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, TinkarTerm.STATUS_VALUE.asUuidArray()[0].toString())
                .inferredNavigation(null, null)
                .statedNavigation(null, null)
                .statedDefinition(TinkarTerm.ROOT_VERTEX)
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
