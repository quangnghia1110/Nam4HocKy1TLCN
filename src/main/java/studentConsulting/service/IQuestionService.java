package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;

import java.util.List;

public interface IQuestionService {

    // Tạo câu hỏi mới
    DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest);

    // Cập nhật câu hỏi
    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);

    // Xóa câu hỏi
    DataResponse<Void> deleteQuestion(Integer questionId);

    // Hỏi tiếp theo
    DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file);

    // Tìm câu hỏi theo người dùng với phân trang
    Page<MyQuestionDTO> getQuestionsByUserId(Integer userId, Pageable pageable);

    // Tìm câu hỏi theo tiêu đề với phân trang
    Page<MyQuestionDTO> searchQuestionsByTitle(Integer userId, String title, Pageable pageable);

    // Lọc câu hỏi theo phòng ban với phân trang
    Page<MyQuestionDTO> filterQuestionsByDepartment(Integer userId, Integer departmentId, Pageable pageable);

    // Tìm câu hỏi đã trả lời với phân trang
    Page<MyQuestionDTO> findAnsweredQuestions(Integer userId, Pageable pageable);

    // Tìm câu hỏi chưa trả lời với phân trang
    Page<MyQuestionDTO> findNotAnsweredQuestions(Integer userId, Pageable pageable);

    // Lọc theo trạng thái công khai hoặc riêng tư với phân trang
    Page<MyQuestionDTO> findByUserIdAndStatusPublic(Integer userId, Boolean isPublic, Pageable pageable);

    // Lọc theo trạng thái duyệt hoặc chưa duyệt với phân trang
    Page<MyQuestionDTO> findByUserIdAndStatusApproval(Integer userId, Boolean isApproved, Pageable pageable);

    // Lọc theo trạng thái xóa hoặc chưa xóa với phân trang
    Page<MyQuestionDTO> findByUserIdAndStatusDelete(Integer userId, Boolean isDeleted, Pageable pageable);

    // Lấy thông tin vai trò hỏi
    List<RoleAskDTO> getAllRoleAsk();

    // Tìm sinh viên theo mã sinh viên
    UserInformationEntity findStudentCode(String studentCode);

    // Tìm phòng ban theo ID
    DepartmentEntity findDepartmentById(Integer id);

    // Tìm lĩnh vực theo ID
    FieldEntity findFieldById(Integer id);

    // Tìm RoleAsk theo ID
    RoleAskEntity findRoleAskById(Integer id);
}
