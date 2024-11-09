package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.DeletionLogDTO;
import studentConsulting.model.payload.dto.actor.MyQuestionDTO;
import studentConsulting.model.payload.dto.actor.QuestionDTO;
import studentConsulting.model.payload.request.CreateQuestionRequest;
import studentConsulting.model.payload.request.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.time.LocalDate;

public interface IQuestionService {

    DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId);

    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);

    DataResponse<Void> deleteQuestion(Integer questionId, String username);

    DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file, Integer userId);

    DataResponse<String> deleteQuestion(Integer questionId, String reason, String username);

    public Page<MyQuestionDTO> getQuestionAnswerByRole(Boolean statusApproval, UserInformationEntity user, String title, String status, Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    public Page<DeletionLogDTO> getDeletionLogs(UserInformationEntity user, Pageable pageable);

    public DeletionLogDTO getDeletionLogDetail(UserInformationEntity user, Integer questionId);

    public MyQuestionDTO getQuestionDetail(Integer consultantId, Integer questionId);
}
