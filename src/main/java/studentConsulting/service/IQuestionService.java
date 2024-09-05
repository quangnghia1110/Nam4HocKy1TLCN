package studentConsulting.service;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

@Service
public interface IQuestionService  {
    DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest);    UserInformationEntity findStudentCode(String studentCode);
    DepartmentEntity findDepartmentById(Integer id);
    FieldEntity findFieldById(Integer id);
    RoleAskEntity findRoleAskById(Integer id);
    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);
    DataResponse<Void> deleteQuestion(Integer questionId);
    List<RoleAskDTO> getAllRoleAsk();
}
