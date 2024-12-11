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
    @Mapping(source = "title", target = "title")
    @Mapping(source = "content", target = "content")
    @Mapping(source = "answerTitle", target = "answerTitle")
    @Mapping(source = "answerContent", target = "answerContent")
    @Mapping(source = "file", target = "file")
    @Mapping(source = "fileAnswer", target = "fileAnswer")
    @Mapping(source = "status", target = "status")
    @Mapping(source = "createdAt", target = "createdAt")
    @Mapping(source = "createdBy", target = "createdBy", qualifiedByName = "mapCreatedBy")
    CommonQuestionDTO mapToDTO(CommonQuestionEntity question);

    @Named("mapCreatedBy")
    default CommonQuestionDTO.CreatedByDTO mapCreatedBy(UserInformationEntity user) {
        if (user == null) {
            return null;
        }
        return CommonQuestionDTO.CreatedByDTO.builder()
                .id(user.getId())
                .name(user.getLastName() + " " + user.getFirstName())
                .build();
    }
}
