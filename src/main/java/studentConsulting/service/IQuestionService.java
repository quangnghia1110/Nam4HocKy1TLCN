package studentConsulting.service;

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

    // Lấy danh sách tất cả câu hỏi với phân trang
    Page<MyQuestionDTO> getAllQuestionsFilters(Pageable pageable);

    // Lọc tất cả câu hỏi theo phòng ban với phân trang
    Page<MyQuestionDTO> getAllQuestionsByDepartmentFilters(Integer departmentId, Pageable pageable);

    // Tạo mới câu hỏi
    DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId);

    // Cập nhật câu hỏi
    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);

    // Xóa câu hỏi
    DataResponse<Void> deleteQuestion(Integer questionId, String username);

    // Xóa câu hỏi với lý do và tên người dùng
    DataResponse<String> deleteQuestion(Integer questionId, String reason, String username);

    // Hỏi tiếp theo (follow-up question)
    DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file, Integer userId);

    // Lọc câu hỏi theo bộ lọc người dùng
    Page<MyQuestionDTO> getQuestionsWithConsultantFilters(Integer consultantId, String title, String status, Pageable pageable);

    // Lọc câu hỏi theo phòng ban và tiêu đề, trạng thái
    Page<MyQuestionDTO> getDepartmentConsultantsQuestionsFilters(Integer departmentId, String title, String status, Pageable pageable);

    // Lọc câu hỏi theo bộ lọc của người dùng (nâng cao)
    Page<MyQuestionDTO> getQuestionsWithUserFilters(Integer userId, String title, String status, Integer departmentId, Pageable pageable);

    // Lấy danh sách vai trò hỏi (RoleAsk)
    List<RoleAskDTO> getAllRoleAsk();

    // Tìm thông tin phòng ban theo ID
    DepartmentEntity findDepartmentById(Integer id);

    // Tìm thông tin lĩnh vực theo ID
    FieldEntity findFieldById(Integer id);

    // Tìm thông tin RoleAsk theo ID
    RoleAskEntity findRoleAskById(Integer id);

    // Chuyển tiếp câu hỏi
    DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest, String username);

    // Lấy danh sách câu hỏi đã chuyển tiếp theo phòng ban và tiêu đề
    Page<ForwardQuestionDTO> getForwardedQuestionsByDepartmentFilters(String title, Integer toDepartmentId, Pageable pageable);

    // Lấy danh sách log xóa theo tên đầy đủ của tư vấn viên
    Page<DeletionLogDTO> getDeletedQuestionsByConsultantFilters(String fullName, Pageable pageable);
}
