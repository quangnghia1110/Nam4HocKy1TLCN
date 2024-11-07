package studentConsulting.model.payload.mapper.actor;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import studentConsulting.model.entity.CommonQuestionEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;

@Mapper(componentModel = "spring")
public interface CommonQuestionMapper {

    @Mapping(source = "id", target = "commonQuestionId")
    @Mapping(source = "department.id", target = "department.id")
    @Mapping(source = "department.name", target = "department.name")
    @Mapping(source = "field.id", target = "field.id")
    @Mapping(source = "field.name", target = "field.name")
    @Mapping(source = "roleAsk.id", target = "roleAsk.id")
    @Mapping(source = "roleAsk.name", target = "roleAsk.name")
    @Mapping(source = "user.firstName", target = "askerFirstname")
    @Mapping(source = "user.lastName", target = "askerLastname")
    @Mapping(source = "answerTitle", target = "answerTitle")
    @Mapping(source = "answerContent", target = "answerContent")
    @Mapping(source = "answerUserEmail", target = "answerUserEmail")
    @Mapping(source = "user.firstName", target = "answerUserFirstname")
    @Mapping(source = "user.lastName", target = "answerUserLastname")
    @Mapping(source = "answerCreatedAt", target = "answerCreatedAt")
    @Mapping(source = "views", target = "views")
    @Mapping(source = "createdAt", target = "createdAt")
    @Mapping(source = "fileName", target = "fileName")
    @Mapping(source = "createdBy", target = "createdBy", qualifiedByName = "mapCreatedByName")
    CommonQuestionDTO mapToDTO(CommonQuestionEntity question);

    @Named("mapCreatedByName")
    default String mapCreatedByName(UserInformationEntity user) {
        if (user == null) return null;
        return user.getLastName() + " " + user.getFirstName();
    }
}
