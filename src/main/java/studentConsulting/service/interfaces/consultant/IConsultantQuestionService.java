package studentConsulting.service.interfaces.consultant;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.user.RoleAskEntity;
import studentConsulting.model.payload.dto.question_answer.DeletionLogDTO;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.request.question_answer.ForwardQuestionRequest;
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
