package studentConsulting.model.payload.mapper.actor;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import studentConsulting.model.entity.AnswerEntity;
import studentConsulting.model.payload.dto.actor.AnswerDTO;

//dùng abstract vì có dependency injection
@Mapper(componentModel = "spring")
public interface AnswerMapper {
    @Mapping(source = "id", target = "answerId")
    @Mapping(source = "question.id", target = "questionId")
    @Mapping(source = "roleConsultant.id", target = "roleConsultantId")
    @Mapping(source = "user.id", target = "userId")
    @Mapping(source = "title", target = "title")
    @Mapping(source = "content", target = "content")
    @Mapping(source = "file", target = "file")
    @Mapping(source = "createdAt", target = "createdAt")
    @Mapping(source = "statusApproval", target = "statusApproval")
    @Mapping(source = "statusAnswer", target = "statusAnswer")
    AnswerDTO mapToAnswerDTO(AnswerEntity answer);
}
