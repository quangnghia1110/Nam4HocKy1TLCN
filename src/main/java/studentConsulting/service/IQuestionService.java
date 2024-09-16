package studentConsulting.service;

import java.time.LocalDate;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;
import studentConsulting.model.payload.dto.DeletionLogDTO;
import studentConsulting.model.payload.dto.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.ForwardQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

public interface IQuestionService {

    DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId);

    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);

    DataResponse<Void> deleteQuestion(Integer questionId, String username);

    DataResponse<String> deleteQuestion(Integer questionId, String reason, String username);

    DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file, Integer userId);

    List<RoleAskDTO> getAllRoleAsk();

    DepartmentEntity findDepartmentById(Integer id);

    FieldEntity findFieldById(Integer id);

    RoleAskEntity findRoleAskById(Integer id);

    DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest, String username);

    Page<MyQuestionDTO> getQuestionsWithUserFilters(Integer userId, String title, String status, Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<MyQuestionDTO> getQuestionsWithConsultantFilters(Integer consultantId, String title, String status, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<MyQuestionDTO> getAllQuestionsByDepartmentFilters(Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<MyQuestionDTO> getAllQuestionsFilters(LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<DeletionLogDTO> getDeletedQuestionsByConsultantFilters(String fullName, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<ForwardQuestionDTO> getForwardedQuestionsByDepartmentFilters(String title, Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<MyQuestionDTO> getDepartmentConsultantsQuestionsFilters(Integer departmentId, String title, String status, LocalDate startDate, LocalDate endDate, Pageable pageable);
}
