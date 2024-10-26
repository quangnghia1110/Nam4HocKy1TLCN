package studentConsulting.service.interfaces.user;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.entity.user.RoleAskEntity;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.QuestionDTO;
import studentConsulting.model.payload.dto.user.RoleAskDTO;
import studentConsulting.model.payload.request.question_answer.CreateQuestionRequest;
import studentConsulting.model.payload.request.question_answer.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.time.LocalDate;
import java.util.List;

public interface IUserQuestionService {

    DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId);

    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);

    DataResponse<Void> deleteQuestion(Integer questionId, String username);

    DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file, Integer userId);

    List<RoleAskDTO> getAllRoleAsk();

    DepartmentEntity findDepartmentById(Integer id);

    FieldEntity findFieldById(Integer id);

    RoleAskEntity findRoleAskById(Integer id);

    Page<MyQuestionDTO> getAllQuestionsByDepartmentFilters(Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<MyQuestionDTO> getAllQuestionsFilters(LocalDate startDate, LocalDate endDate, Pageable pageable);
}
