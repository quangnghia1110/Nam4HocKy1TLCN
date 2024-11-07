package studentConsulting.model.payload.mapper.actor;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import studentConsulting.model.entity.ForwardQuestionEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.ForwardQuestionDTO;

@Mapper(componentModel = "spring")
public interface ForwardQuestionMapper {

    @Mapping(source = "id", target = "id")
    @Mapping(source = "title", target = "title")
    @Mapping(source = "fromDepartment.id", target = "fromDepartment.id")
    @Mapping(source = "fromDepartment.name", target = "fromDepartment.name")
    @Mapping(source = "toDepartment.id", target = "toDepartment.id")
    @Mapping(source = "toDepartment.name", target = "toDepartment.name")
    @Mapping(source = "statusForward", target = "statusForward")
    @Mapping(source = "createdBy.id", target = "createdBy")
    @Mapping(source = "consultant", target = "consultant", qualifiedByName = "mapConsultant")
    ForwardQuestionDTO mapToDTO(ForwardQuestionEntity forwardQuestion, @Context Integer consultantId);

    @Named("mapConsultant")
    default ForwardQuestionDTO.ConsultantDTO mapConsultant(UserInformationEntity consultant) {
        if (consultant == null) return null;
        return ForwardQuestionDTO.ConsultantDTO.builder()
                .id(consultant.getId())
                .firstName(consultant.getFirstName())
                .lastName(consultant.getLastName())
                .build();
    }
}
