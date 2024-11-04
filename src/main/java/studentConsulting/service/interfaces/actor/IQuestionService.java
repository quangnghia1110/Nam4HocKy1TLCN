package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.question_answer.DeletionLogDTO;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.QuestionDTO;
import studentConsulting.model.payload.dto.user.RoleAskDTO;
import studentConsulting.model.payload.request.question_answer.CreateQuestionRequest;
import studentConsulting.model.payload.request.question_answer.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.time.LocalDate;
import java.util.List;

public interface IQuestionService {

    DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId);

    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);

    DataResponse<Void> deleteQuestion(Integer questionId, String username);

    DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file, Integer userId);

    public Page<MyQuestionDTO> getAllQuestionsWithFilters(Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    DataResponse<String> deleteQuestion(Integer questionId, String reason, String username);

    Page<MyQuestionDTO> getQuestionAnswerByRole(UserInformationEntity user, String title, String status, Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    Page<MyQuestionDTO> getDepartmentConsultantsQuestionsFilters(Integer departmentId, String title, String status, LocalDate startDate, LocalDate endDate, Pageable pageable);

    public Page<DeletionLogDTO> getDeletionLogs(UserInformationEntity user, Pageable pageable);

    void importQuestions(List<List<String>> csvData);

    public List<RoleAskDTO> getAllRoleAsk();

    public DeletionLogDTO getDeletionLogDetail(UserInformationEntity user, Integer questionId);

    public MyQuestionDTO getQuestionDetail(Integer consultantId, Integer questionId);
}
