package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.request.question.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.util.List;

public interface IAnswerService {
    public AnswerDTO createAnswer(CreateAnswerRequest request);
}
