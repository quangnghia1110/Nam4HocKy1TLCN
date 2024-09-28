package studentConsulting.service.interfaces.consultant;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.questionAnswer.DeletionLogEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;
import studentConsulting.model.payload.dto.DeletionLogDTO;
import studentConsulting.model.payload.dto.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.request.question.ForwardQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.time.LocalDate;

public interface IConsultantQuestionService {


    DataResponse<String> deleteQuestion(Integer questionId, String reason, String username);

    DepartmentEntity findDepartmentById(Integer id);

    FieldEntity findFieldById(Integer id);

    RoleAskEntity findRoleAskById(Integer id);

    DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest, String username);

    Page<MyQuestionDTO> getQuestionsWithConsultantFilters(Integer consultantId, String title, String status, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<DeletionLogDTO> getDeletedQuestionsByConsultantFilters(String fullName, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<ForwardQuestionDTO> getForwardedQuestionsByDepartmentFilters(String title, Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<DeletionLogEntity> getDeletionLogsByConsultant(Integer consultantId, Pageable pageable);
}
