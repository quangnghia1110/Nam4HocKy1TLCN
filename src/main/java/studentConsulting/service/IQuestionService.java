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
	
	//Danh sách câu hỏi
    Page<MyQuestionDTO> getAllQuestions(Pageable pageable);

    // Tạo câu hỏi mới
	public DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId);

    // Cập nhật câu hỏi
    DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request);

    // Xóa câu hỏi
	DataResponse<Void> deleteQuestion(Integer questionId, String username);
	
    // Hỏi tiếp theo
	public DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file, Integer userId);
    // Lọc tất cả câu hỏi theo phòng ban với phân trang
    Page<MyQuestionDTO> filterAllQuestionsByDepartment(Integer departmentId, Pageable pageable);

    
    

    
    
    
	public Page<MyQuestionDTO> getQuestionsByDepartment(Integer departmentId, String title, String status, Pageable pageable);


	Page<MyQuestionDTO> getQuestionsWithFilters(Integer userId, String title, String status, Pageable pageable);
    
    // Lấy thông tin vai trò hỏi
    List<RoleAskDTO> getAllRoleAsk();


    // Tìm phòng ban theo ID
    DepartmentEntity findDepartmentById(Integer id);

    // Tìm lĩnh vực theo ID
    FieldEntity findFieldById(Integer id);

    // Tìm RoleAsk theo ID
    RoleAskEntity findRoleAskById(Integer id);
	UserInformationEntity findUserById(Integer id);
    
    
    public Page<MyQuestionDTO> getQuestionsWithUserFilters(Integer userId,String title,String status,Integer departmentId,Pageable pageable);	public Page<ForwardQuestionDTO> getForwardedQuestionsByDepartment(String title, Integer toDepartmentId, Pageable pageable);
		DataResponse<String> deleteQuestion(Integer questionId, String reason, String username);
		public Page<DeletionLogDTO> getDeletedQuestionsByConsultantFullName(String fullName, Pageable pageable);

	DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest, String username);
}
