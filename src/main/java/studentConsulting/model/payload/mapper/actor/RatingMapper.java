package studentConsulting.model.payload.mapper.actor;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import studentConsulting.model.entity.RatingEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.RatingDTO;

@Mapper(componentModel = "spring")
public interface RatingMapper {

    @Mapping(source = "id", target = "id")
    @Mapping(source = "department.id", target = "department.id")
    @Mapping(source = "department.name", target = "department.name")
    @Mapping(source = "user", target = "user", qualifiedByName = "mapUser")
    @Mapping(source = "consultant", target = "consultant", qualifiedByName = "mapConsultant")
    @Mapping(source = "generalSatisfaction", target = "generalSatisfaction")
    @Mapping(source = "generalComment", target = "generalComment")
    @Mapping(source = "expertiseKnowledge", target = "expertiseKnowledge")
    @Mapping(source = "expertiseComment", target = "expertiseComment")
    @Mapping(source = "attitude", target = "attitude")
    @Mapping(source = "attitudeComment", target = "attitudeComment")
    @Mapping(source = "responseSpeed", target = "responseSpeed")
    @Mapping(source = "responseSpeedComment", target = "responseSpeedComment")
    @Mapping(source = "understanding", target = "understanding")
    @Mapping(source = "understandingComment", target = "understandingComment")
    @Mapping(source = "submittedAt", target = "submittedAt")
    RatingDTO mapToDTO(RatingEntity rating);

    @Named("mapUser")
    default RatingDTO.UserDTO mapUser(UserInformationEntity user) {
        if (user == null) return null;
        return RatingDTO.UserDTO.builder()
                .id(user.getId())
                .name(user.getLastName() + " " + user.getFirstName())
                .build();
    }

    @Named("mapConsultant")
    default RatingDTO.UserDTO mapConsultant(UserInformationEntity consultant) {
        if (consultant == null) return null;
        return RatingDTO.UserDTO.builder()
                .id(consultant.getId())
                .name(consultant.getLastName() + " " + consultant.getFirstName())
                .build();
    }
}
