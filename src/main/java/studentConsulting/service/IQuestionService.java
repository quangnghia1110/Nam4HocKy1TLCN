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
	
	//Danh sách câu hỏi
    Page<MyQuestionDTO> getAllQuestions(Pageable pageable);

    // Tạo câu hỏi mới
    DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest);

    // Cập nhật câu hỏi
    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);

    // Xóa câu hỏi
    DataResponse<Void> deleteQuestion(Integer questionId);

    // Hỏi tiếp theo
    DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file);

    // Lọc tất cả câu hỏi theo phòng ban với phân trang
    Page<MyQuestionDTO> filterAllQuestionsByDepartment(Integer departmentId, Pageable pageable);

    
    
   
    
    Page<MyQuestionDTO> findAnsweredQuestionsByTitleAndDepartment(Integer userId, String title, Integer departmentId, Pageable pageable);

    Page<MyQuestionDTO> findNotAnsweredQuestionsByTitleAndDepartment(Integer userId, String title, Integer departmentId, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusPublicTitleAndDepartment(Integer userId, boolean isPublic, String title, Integer departmentId, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusDeleteTitleAndDepartment(Integer userId, boolean isDeleted, String title, Integer departmentId, Pageable pageable);
    
    Page<MyQuestionDTO> findByUserIdAndStatusApprovalTitleAndDepartment(Integer userId, boolean isApproved, String title, Integer departmentId, Pageable pageable);

    
    
    
    
    Page<MyQuestionDTO> searchQuestionsByTitle(Integer userId, String title, Pageable pageable);
    
    Page<MyQuestionDTO> findNotAnsweredQuestionsByTitle(Integer userId, String title, Pageable pageable);

    Page<MyQuestionDTO> findAnsweredQuestionsByTitle(Integer userId, String title, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusPublicTitle(Integer userId, boolean isPublic, String title, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusDeleteTitle(Integer userId, boolean isDeleted, String title, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusApprovalTitle(Integer userId, boolean isApproved, String title, Pageable pageable);


    



    Page<MyQuestionDTO> findAnsweredQuestionsByDepartment(Integer userId, Integer departmentId, Pageable pageable);

    Page<MyQuestionDTO> findNotAnsweredQuestionsByDepartment(Integer userId, Integer departmentId, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusPublicAndDepartment(Integer userId, boolean isPublic, Integer departmentId, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusDeleteAndDepartment(Integer userId, Boolean isDeleted, Integer departmentId, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusApprovalAndDepartment(Integer userId, Boolean isApproved, Integer departmentId, Pageable pageable);

    
    
    
    
    Page<MyQuestionDTO> findAnsweredQuestions(Integer userId, Pageable pageable);

    Page<MyQuestionDTO> findNotAnsweredQuestions(Integer userId, Pageable pageable);    
    
    Page<MyQuestionDTO> findByUserIdAndStatusPublic(Integer userId, boolean isPublic, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusDelete(Integer userId, boolean isDeleted, Pageable pageable);

    Page<MyQuestionDTO> findByUserIdAndStatusApproval(Integer userId, boolean isApproved, Pageable pageable);

    
    
    
    
    
    Page<MyQuestionDTO> getQuestionsByUserId(Integer userId, Pageable pageable);

    Page<MyQuestionDTO> searchQuestionsByTitleAndDepartment(Integer userId, String title, Integer departmentId, Pageable pageable);

    Page<MyQuestionDTO> filterMyQuestionsByDepartment(Integer userId, Integer departmentId, Pageable pageable);
    
    
    
    
   

	Page<MyQuestionDTO> getQuestionsWithFilters(Integer userId, String title, String status, Pageable pageable);
    
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
